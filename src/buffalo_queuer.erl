-module(buffalo_queuer).
-behaviour(gen_server).

-include("../include/buffalo.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, queue/4, queue/5, cancel/1, cancel/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

cancel(Key) ->
    gen_server:call(?SERVER, {cancel, Key}).

cancel(Module, Function, Arguments) ->
    cancel(key(Module, Function, Arguments)).

queue(Module, Function, Arguments, Timeout) ->
    queue(key(Module, Function, Arguments), Module, Function, Arguments, Timeout).

queue(Key, Module, Function, Arguments, Timeout) ->
    gen_server:call(?SERVER, {queue, Key, {Module, Function, Arguments}, Timeout}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, []}.

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

handle_call({queue, Key, MFA, Timeout}, _From, State) ->
    {ok, Ret} = add_mfa(Key, MFA, Timeout),
    {reply, {ok, Ret}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Key}, State) ->
    ok = start_key(Key),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

key(M, F, A) ->
    {M, F, A}.

start_key(Key) ->
    case ets:lookup(buffalo, Key) of
        [] ->
            ok;
        [#buffalo_entry{mfa=MFA}] ->
            start_mfa(MFA),
            ets:delete(buffalo, Key),
            ok
    end.

start_mfa(MFA) ->
    {ok, _Pid} = supervisor:start_child(buffalo_worker_sup, [MFA]).


add_mfa(Key, MFA, Timeout) ->
    add_mfa_1(ets:lookup(buffalo, Key), current_msecs(), Key, MFA, Timeout).


add_mfa_1([#buffalo_entry{key=Key, timer=OldRef, deadline=Deadline}], Now, Key, MFA, _Timeout) when Deadline =< Now ->
    erlang:cancel_timer(OldRef),
    start_mfa(MFA),
    ets:delete(buffalo, Key),
    {ok, existing};
add_mfa_1([#buffalo_entry{key=Key, timer=OldRef, deadline=Deadline} = Entry], Now, Key, MFA, Timeout) ->
    erlang:cancel_timer(OldRef),
    NextTimeout = erlang:min(Timeout, Deadline-Now),
    NewRef = erlang:send_after(NextTimeout, self(), {timeout, MFA}),
    ets:insert(buffalo, Entry#buffalo_entry{timer=NewRef, mfa=MFA}),
    {ok, existing};
add_mfa_1([], Now, Key, MFA, Timeout) ->
    NewRef = erlang:send_after(Timeout, self(), {timeout, MFA}),
    ets:insert(buffalo, #buffalo_entry{key=Key, mfa=MFA, timer=NewRef, deadline=Now+Timeout*?DEADLINE_MULTIPLIER}),
    {ok, new}.


current_msecs() ->
    {A,B,C} = os:timestamp(),
    ((A * 1000000) + B * 1000) + C div 1000.

