
-module(buffalo_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Worker = {buffalo_worker,
              {buffalo_worker, start_link, []},
              temporary,
              2000,
              worker,
              [buffalo_worker]},
    {ok, {{simple_one_for_one, 0, 1}, [Worker]}}.
