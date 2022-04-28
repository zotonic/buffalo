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

%% @doc Start application `buffalo'.

-spec start() -> Result when
	Result :: ok | {error, Reason},
	Reason :: term().
start() ->
    application:start(buffalo).

%% @doc Call the MFA after specified in `Options' time.

-spec queue(MFA, Opts) -> Result when
	MFA :: mfargs(),
	Opts :: options(),
	Result :: {ok, existing | new | running}.
queue(MFA, Opts) ->
    buffalo_queuer:queue(MFA, Opts).

%% @doc Call the MFA after specified in `Options' time 
%%	using specified `Key'.

-spec queue(Key, MFA, Opts) -> Result when
	Key :: key(), 
	MFA :: mfargs(), 
	Opts :: options(),
	Result :: {ok, existing | new | running}.
queue(Key, MFA, Opts) ->
    buffalo_queuer:queue(Key, MFA, Opts).

%% @doc Cancel the MFA running.
-spec cancel(Key) -> Result when
	Key :: mfargs() | key(),
	Result :: ok | {error, notfound}.
cancel({Module, Function, Arguments} = MFA) when is_atom(Module), is_atom(Function), is_list(Arguments) ->
    buffalo_queuer:cancel_mfa(MFA);
cancel(Key) ->
    buffalo_queuer:cancel_key(Key).

%% @doc Check if the task with the specified key is running or queued.

-spec status(Key) -> Result when
	Key :: key(),
	Result :: {ok, queued | running} | {error, notfound}.
status(Key) ->
    buffalo_queuer:status(Key).

%% @deprecated Deprecated API

-spec queue(Module, Function, Arguments, Timeout) -> Result when
	Module :: atom(), 
	Function :: atom(), 
	Arguments :: list(), 
	Timeout :: pos_integer(),
	Result :: {ok, existing | new}.
queue(Module, Function, Arguments, Timeout) when is_integer(Timeout) ->
    buffalo_queuer:queue({Module, Function, Arguments}, #{ timeout => Timeout }).

%% @deprecated Deprecated API

-spec queue(Key, Module, Function, Arguments, Timeout) -> Result when
	Key :: key(), 
	Module :: atom(), 
	Function :: atom(), 
	Arguments :: list(), 
	Timeout :: pos_integer(),
	Result :: {ok, existing | new}.
queue(Key, Module, Function, Arguments, Timeout) when is_integer(Timeout) ->
    buffalo_queuer:queue(Key, {Module, Function, Arguments}, #{ timeout => Timeout }).

%% @deprecated Deprecated API

-spec cancel(Module, Function, Arguments) -> Result when 
	Module :: atom(), 
	Function :: atom(), 
	Arguments :: list(),
	Result :: ok | {error, notfound}.
cancel(Module, Function, Arguments) ->
    buffalo_queuer:cancel_mfa({Module, Function, Arguments}).
