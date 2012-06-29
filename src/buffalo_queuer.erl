-module(buffalo_queuer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, queue/4]).

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

queue(Module, Function, Arguments, Timeout) ->
    gen_server:call(?SERVER, {queue, {Module, Function, Arguments}, Timeout}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, []}.

handle_call({queue, MFA, Timeout}, _From, All) ->
    {Ret, List} = case lists:keyfind(MFA, 2, All) of
              {OldRef, _} ->
                          erlang:cancel_timer(OldRef),
                          {existing, lists:keydelete(MFA, 2, All)};
                      false ->
                          {new, All}
                  end,
    Ref = erlang:send_after(Timeout, self(), {timeout, MFA}),
    {reply, {ok, Ret}, [{Ref, MFA}|List]}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, MFA}, All) ->
    {ok, _Pid} = supervisor:start_child(buffalo_worker_sup, [MFA]),
    New = lists:keydelete(MFA, 2, All),
    {noreply, New}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

