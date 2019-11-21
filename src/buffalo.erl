%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2019 Arjan Scherpenisse
%% @doc Buffalo main API

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

-module(buffalo).
-export([start/0, queue/4, queue/5, cancel/1, cancel/3]).

%% API

start() ->
    application:start(buffalo).

queue(Module, Function, Arguments, Timeout) ->
    buffalo_queuer:queue(Module, Function, Arguments, Timeout).

queue(Key, Module, Function, Arguments, Timeout) ->
    buffalo_queuer:queue(Key, Module, Function, Arguments, Timeout).

cancel(Module, Function, Arguments) ->
    buffalo_queuer:cancel(Module, Function, Arguments).

cancel(Key) ->
    buffalo_queuer:cancel(Key).
