%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2019 Arjan Scherpenisse
%% @doc Buffalo queuer: buffers, deduplicates, and starts workers<br/>
%% Implementation of `gen_server' behaviour.
%% @end

%% Copyright 2012-2019 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(buffalo_queuer).
-behaviour(gen_server).

-include("../include/buffalo.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, queue/2, queue/3, cancel_key/1, cancel_mfa/1, status/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    pid_to_key = #{} :: #{ pid() := buffalo:mfargs() },
    key_to_pid = #{} :: #{ buffalo:mfargs() := pid() }
}).


-type state() :: #state{
						pid_to_key :: #{ pid() := buffalo:mfargs() },
						key_to_pid :: #{ buffalo:mfargs() := pid() }
					}.
					
%% @private
-type buffalo_entry() :: #buffalo_entry{
							key :: buffalo:key(),
							mfa :: buffalo:mfargs(),
							timer :: reference(), 
							deadline :: non_neg_integer()
						}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Creates a `gen_server' process as part of a supervision tree.

-spec start_link() -> Result when
	Result :: {ok,Pid} | ignore | {error,Error},
	Pid :: pid(),
	Error :: {already_started,Pid} | term().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Cancel the MFA running using specified `Key'.

-spec cancel_key(Key) -> Result when
	Key :: buffalo:key(),
	Result :: ok | {error, notfound}.
cancel_key(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

%% @doc Cancel the MFA running.
%% @equiv cancel_key(key(MFA))

-spec cancel_mfa(MFA) -> Result when
	MFA :: buffalo:mfargs(),
	Result :: ok | {error, notfound}.
cancel_mfa(MFA) ->
    cancel_key(key(MFA)).

%% @doc Call the MFA after specified in `Options' time.
%% @equiv queue(key(MFA), MFA, Options)

-spec queue(MFA, Options) -> Result when
	MFA :: buffalo:mfargs(), 
	Options :: buffalo:options(),
	Result :: {ok, new | existing}.
queue(MFA, Options) ->
    queue(key(MFA), MFA, Options).

%% @doc Call the MFA after specified in `Options' time 
%%	using specified `Key'.

-spec queue(Key, MFA, Options) -> Result when
	Key :: buffalo:key(), 
	MFA :: buffalo:mfargs(), 
	Options :: buffalo:options(),
	Result :: {ok, new | existing}.
queue(Key, MFA, Options) ->
    gen_server:call(?SERVER, {queue, Key, MFA, Options}).

%% @doc Check if the task with the specified key is running or queued.

-spec status(Key) -> Result when
	Key :: buffalo:key(),
	Result :: {ok, queued | running} | {error, notfound}.
status(Key) ->
    gen_server:call(?SERVER, {status, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private

-spec init(Args) -> Result when 
	Args :: list(),
	Result :: {ok, state()}.
init(_Args) ->
    {ok, #state{
        key_to_pid = #{},
        pid_to_key = #{}
    }}.

%% @private

-spec handle_call(Request, From, State) -> Result when 
	Request :: {cancel, Key},
	Key :: buffalo:key(),
	From :: {pid(), atom()},
    State :: state(),
	Result :: {reply, Reply, State},
	Reply :: ok | {error, notfound};
(Request, From, State) -> Result when 
	Request :: {queue, Key, MFA, Options},
	Key :: buffalo:key(),
	MFA :: buffalo:mfargs(),
	Options :: buffalo:options(),
	From :: {pid(), atom()},
    State :: state(),
	Result :: {reply, Reply, State},
	Reply :: {ok, running} | {ok, existing} | {ok, new};
(Request, From, State) -> Result when 
	Request :: {status, Key},
	Key :: buffalo:key(),
	From :: {pid(), atom()},
    State :: state(),
	Result :: {reply, Reply, State},
	Reply :: {ok, queued} | {ok, running} | {error, notfound}.
handle_call({cancel, Key}, _From, State) ->
    Ret = case ets:lookup(buffalo, Key) of
              [#buffalo_entry{key=Key, timer=OldRef}] ->
                  erlang:cancel_timer(OldRef),
                  ets:delete(buffalo, Key),
                  ok;
              [] ->
                  {error, notfound}
          end,
    {reply, Ret, State};

handle_call({queue, Key, MFA, #{ is_drop_running := true } = Options}, _From, State) ->
    Ret = case maps:is_key(Key, State#state.key_to_pid) of
        true ->
            {ok, running};
        false ->
            add_mfa(Key, MFA, Options)
    end,
    {reply, Ret, State};

handle_call({queue, Key, MFA, Options}, _From, State) ->
    Ret = add_mfa(Key, MFA, Options),
    {reply, Ret, State};

handle_call({status, Key}, _From, #state{ key_to_pid = KeyToPid } = State) ->
    Ret = case ets:lookup(buffalo, Key) of
        [ #buffalo_entry{} ] ->
                    {ok, queued};
        [] ->
            case maps:is_key(Key, KeyToPid) of
                true -> {ok, running};
                false -> {error, notfound}
            end
    end,
    {reply, Ret, State}.

%% @private

-spec handle_cast(Request, State) -> Result when 
	Request :: term(),
    State :: state(),
	Result :: {noreply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private

-spec handle_info(Info, State) -> Result when 
    Info :: {'DOWN', Ref, process, Pid, ExitStatus},
    State :: state(),
	Ref :: reference(),
	Pid :: pid(),
	ExitStatus :: term(),
    Result :: {noreply, State};
(Info, State) -> Result when 
    Info :: {timeout, Key},
	Key :: buffalo:mfargs(),
    State :: state(),
    Result :: {noreply, State}.
handle_info({'DOWN', _Ref, process, Pid, _ExitStatus}, State) ->
    case maps:get(Pid, State#state.pid_to_key, undefined) of
        undefined ->
            {noreply, State};
        Key ->
            PidToKey1 = maps:remove(Pid, State#state.pid_to_key),
            KeyToPid1 = maps:remove(Key, State#state.key_to_pid),
            {noreply, State#state{ pid_to_key = PidToKey1, key_to_pid = KeyToPid1 }}
    end;

handle_info({timeout, Key}, #state{ pid_to_key = PidToKey, key_to_pid = KeyToPid } = State) ->
    {ok, Pid} = start_key(Key),
    erlang:monitor(process, Pid),
    State1 = State#state{
        pid_to_key = PidToKey#{ Pid => Key },
        key_to_pid = KeyToPid#{ Key => Pid }
    },
    {noreply, State1}.

%% @private

-spec terminate(Reason, State) -> Result when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: state(),
    Result :: ok.
terminate(_Reason, _State) ->
    ok.

%% @private

-spec code_change(OldVersion, State, Extra) -> Result when
	OldVersion :: (term() | {down, term()}),
	State :: state(),
	Extra :: term(),
	Result :: {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec key(buffalo:mfargs()) -> Result when
	Result :: buffalo:mfargs().
key({_M, _F, _A} = MFA) ->
    MFA.

-spec start_key(Key) -> Result when
	Key :: buffalo:mfargs(),
	Result :: {error, notfound} | {ok, Pid},
	Pid :: pid().
start_key(Key) ->
    case ets:lookup(buffalo, Key) of
        [] ->
            {error, notfound};
        [ #buffalo_entry{mfa=MFA} ] ->
            {ok, Pid} = supervisor:start_child(buffalo_worker_sup, [MFA]),
            ets:delete(buffalo, Key),
            {ok, Pid}
    end.

%% @equiv add_mfa_1(ets:lookup(buffalo, Key), current_msecs(), Key, MFA, Options)

-spec add_mfa(Key, MFA, Options) -> Result when
	Key :: buffalo:key(),
	MFA :: buffalo:mfargs(),
	Options :: buffalo:options(),
	Result :: {ok, existing} | {ok, new}.
add_mfa(Key, MFA, Options) ->
    add_mfa_1(ets:lookup(buffalo, Key), current_msecs(), Key, MFA, Options).

-spec add_mfa_1(BuffaloEntryList, Now, Key, MFA, Options) -> Result when
	BuffaloEntryList :: [] | [buffalo_entry()], 
	Now :: non_neg_integer(), 
	Key :: buffalo:key(),
	MFA :: buffalo:mfargs(),
	Options :: buffalo:options(),
	Result :: {ok, existing} | {ok, new}.
add_mfa_1([#buffalo_entry{key=Key, timer=OldRef, deadline=Deadline} = Entry], Now, Key, MFA, Options) ->
    erlang:cancel_timer(OldRef),
    Timeout = timeout(Options),
    NextTimeout = erlang:max( 0, erlang:min(Timeout, Deadline-Now) ),
    NewRef = erlang:send_after(NextTimeout, self(), {timeout, Key}),
    ets:insert(buffalo, Entry#buffalo_entry{timer=NewRef, mfa=MFA}),
    {ok, existing};
add_mfa_1([], Now, Key, MFA, Options) ->
    Timeout = timeout(Options),
    Deadline = deadline(Options),
    NewRef = erlang:send_after(Timeout, self(), {timeout, Key}),
    ets:insert(buffalo, #buffalo_entry{key=Key, mfa=MFA, timer=NewRef, deadline=Now+Deadline}),
    {ok, new}.

-spec timeout(Options) -> Result when
	Options :: buffalo:options(),
	Result :: non_neg_integer().
timeout(#{ timeout := Timeout }) -> erlang:max(0, Timeout);
timeout(_Options) -> ?DEFAULT_TIMEOUT.

-spec deadline(Options) -> Result when
	Options :: buffalo:options(),
	Result :: non_neg_integer().
deadline(#{ deadline := Deadline }) -> Deadline;
deadline(Options) -> timeout(Options) * ?DEADLINE_MULTIPLIER.

-spec current_msecs() -> Result when
	Result :: non_neg_integer().
current_msecs() ->
    {A,B,C} = os:timestamp(),
    ((A * 1000000) + B * 1000) + C div 1000.

