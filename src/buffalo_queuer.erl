%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2019 Arjan Scherpenisse
%% @doc Buffalo queuer: buffers, deduplicates, and starts workers

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

-export([start_link/0, queue/2, queue/3, cancel_key/1, cancel_mfa/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    pid_to_key = #{} :: #{ pid() := buffalo:mfargs() },
    key_to_pid = #{} :: #{ buffalo:mfargs() := pid() }
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec cancel_key( buffalo:key() ) -> ok | {error, notfound}.
cancel_key(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

-spec cancel_mfa( buffalo:mfargs() ) -> ok | {error, notfound}.
cancel_mfa(MFA) ->
    cancel_key(key(MFA)).

-spec queue( buffalo:mfargs(), buffalo:options() ) -> {ok, new | existing}.
queue(MFA, Options) ->
    queue(key(MFA), MFA, Options).

-spec queue( buffalo:key(), buffalo:mfargs(), buffalo:options() ) -> {ok, new | existing}.
queue(Key, MFA, Options) ->
    gen_server:call(?SERVER, {queue, Key, MFA, Options}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{
        key_to_pid = #{},
        pid_to_key = #{}
    }}.

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
    {reply, Ret, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

key({_M, _F, _A} = MFA) ->
    MFA.

start_key(Key) ->
    case ets:lookup(buffalo, Key) of
        [] ->
            {error, notfound};
        [ #buffalo_entry{mfa=MFA} ] ->
            {ok, Pid} = supervisor:start_child(buffalo_worker_sup, [MFA]),
            ets:delete(buffalo, Key),
            {ok, Pid}
    end.

add_mfa(Key, MFA, Options) ->
    add_mfa_1(ets:lookup(buffalo, Key), current_msecs(), Key, MFA, Options).

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

timeout(#{ timeout := Timeout }) -> erlang:max(0, Timeout);
timeout(_Options) -> ?DEFAULT_TIMEOUT.

deadline(#{ deadline := Deadline }) -> Deadline;
deadline(Options) -> timeout(Options) * ?DEADLINE_MULTIPLIER.


current_msecs() ->
    {A,B,C} = os:timestamp(),
    ((A * 1000000) + B * 1000) + C div 1000.

