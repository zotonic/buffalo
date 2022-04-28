%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2019 Arjan Scherpenisse
%% @doc Buffalo supervisor for task workers<br/>
%% Implementation of `supervisor' behaviour.
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

-spec start_link() -> Result when
	Result :: {ok, pid()} | ignore | {error, StartlinkError},
	StartlinkError :: {already_started, pid()} | {shutdown, term()} | term().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @private

-spec init(Args) -> Result when 
	Args :: [],
	Result :: {ok,{SupFlags, ChildSpec}} | ignore,
	SupFlags :: {simple_one_for_one, 0, 1},
	ChildSpec :: [Worker],
	Worker :: {buffalo_worker,
              {buffalo_worker, start_link, []},
              temporary,
              2000,
              worker,
              [buffalo_worker]}.
init([]) ->
    Worker = {buffalo_worker,
              {buffalo_worker, start_link, []},
              temporary,
              2000,
              worker,
              [buffalo_worker]},
    {ok, {{simple_one_for_one, 0, 1}, [Worker]}}.
