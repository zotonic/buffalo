-module(buffalo_queuer).
-behaviour(gen_server).
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
              [{Key, _MFA, OldRef}] ->
                  erlang:cancel_timer(OldRef),
                  ets:delete(buffalo, Key),
                  ok;
              [] ->
                  {error, notfound}
          end,
    {reply, Ret, State};

handle_call({queue, Key, MFA, Timeout}, _From, State) ->
    Ret = case ets:lookup(buffalo, Key) of
              [{Key, _OldMFA, OldRef}] ->
                  erlang:cancel_timer(OldRef),
                  ets:delete(buffalo, Key),
                  existing;
              [] ->
                  new
          end,
    Ref = erlang:send_after(Timeout, self(), {timeout, Key, MFA}),
    ets:insert(buffalo, {Key, MFA, Ref}),
    {reply, {ok, Ret}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Key, MFA}, State) ->
    {ok, _Pid} = supervisor:start_child(buffalo_worker_sup, [MFA]),
    ets:delete(buffalo, Key),
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
