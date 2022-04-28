%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2019 Arjan Scherpenisse
%% @doc Buffalo application<br/>
%  Implementation of `application' behaviour.
%% @private
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

-module(buffalo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(StartType, StartArgs) -> Result when
	StartType :: application:start_type(), 
	StartArgs :: term(),
	Result :: {ok, pid()} | {error, Reason},
	Reason :: term().
start(_StartType, _StartArgs) ->
    buffalo_sup:start_link().

-spec stop(State) -> Result when
	State :: term(),
	Result :: ok.
stop(_State) ->
    ok.
