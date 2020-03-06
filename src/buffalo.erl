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

-export([
    start/0,
    queue/2, queue/3,
    cancel/1,
    status/1
]).

-deprecated([{queue,4,eventually}]).
-deprecated([{queue,5,eventually}]).
-deprecated([{cancel,3,eventually}]).

-export([
    queue/4, queue/5,
    cancel/3
]).

-type options() :: #{
        timeout => pos_integer(),
        deadline => pos_integer(),
        is_drop_running => boolean()
    }.

-type key() :: term().

-type mfargs() :: {atom(), atom(), list()}.

-export_type([ options/0, key/0, mfargs/0 ]).

%% API

start() ->
    application:start(buffalo).

-spec queue( mfargs(), options() ) -> {ok, existing | new | running}.
queue(MFA, Opts) ->
    buffalo_queuer:queue(MFA, Opts).

-spec queue( key(), mfargs(), options() ) -> {ok, existing | new | running}.
queue(Key, MFA, Opts) ->
    buffalo_queuer:queue(Key, MFA, Opts).

-spec cancel( mfargs() | key() ) -> ok | {error, notfound}.
cancel({Module, Function, Arguments} = MFA) when is_atom(Module), is_atom(Function), is_list(Arguments) ->
    buffalo_queuer:cancel_mfa(MFA);
cancel(Key) ->
    buffalo_queuer:cancel_key(Key).

%% @doc Check if the task with the specified key is running or queued.
-spec status( key() ) -> {ok, queued | running} | {error, notfound}.
status(Key) ->
    buffalo_queuer:status(Key).


%% @doc Deprecated API
-spec queue( atom(), atom(), list(), pos_integer() ) -> {ok, existing | new}.
queue(Module, Function, Arguments, Timeout) when is_integer(Timeout) ->
    buffalo_queuer:queue({Module, Function, Arguments}, #{ timeout => Timeout }).

%% @doc Deprecated API
-spec queue( key(), atom(), atom(), list(), pos_integer() ) -> {ok, existing | new}.
queue(Key, Module, Function, Arguments, Timeout) when is_integer(Timeout) ->
    buffalo_queuer:queue(Key, {Module, Function, Arguments}, #{ timeout => Timeout }).

%% @doc Deprecated API
-spec cancel(atom(), atom(), list()) -> ok | {error, notfound}.
cancel(Module, Function, Arguments) ->
    buffalo_queuer:cancel_mfa({Module, Function, Arguments}).
