%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012-2026 Arjan Scherpenisse
%% @doc Buffalo worker, executes a buffered task<br/>
%% Implementation of `gen_server' behaviour.
%% @end

%% Copyright 2012-2026 Arjan Scherpenisse
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

-module(buffalo_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include_lib("kernel/include/logger.hrl").

-type mfargs() :: buffalo:mfargs().
-type state() :: mfargs().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start a worker with the calling process's logger metadata.
start_link(MFA) ->
    start_link(MFA, logger:get_process_metadata()).

%% @doc Start a worker with the logger metadata captured when the task was queued.
start_link(MFA, Metadata) ->
    gen_server:start_link(?MODULE, {MFA, Metadata}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private

-spec init({MFA, Metadata}) -> Result when
	MFA :: mfargs(),
    Metadata :: logger:metadata() | undefined,
	Result :: {ok, MFA, 0}.
init({MFA, undefined}) ->
    {ok, MFA, 0};
init({MFA, Metadata}) ->
    logger:set_process_metadata(Metadata),
    {ok, MFA, 0}.

%% @private

-spec handle_call(Request, From, State) -> Result when
	Request :: term(),
	From :: gen_server:from(),
    State :: state(),
	Result :: {reply, ok, State}.
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%% @private

-spec handle_cast(Request, State) -> Result when
	Request :: term(),
    State :: state(),
	Result :: {noreply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private

-spec handle_info(Info, State) -> Result when
	Info ::  timeout | term(),
	State :: state(),
	Result :: {stop, normal, undefined} | {noreply, buffalo:mfargs()}.
handle_info(timeout, {Module, Function, Args}) ->
    case erlang:apply(Module, Function, Args) of
        ok ->
            ok;
        {ok, _} ->
            ok;
        Other ->
            ?LOG_WARNING(#{
                in => buffalo,
                text => <<"Non ok result from worker">>,
                result => error,
                reason => reason(Other),
                module => Module,
                function => Function,
                args => Args
            })
    end,
    {stop, normal, undefined};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private

-spec terminate(Reason, State) -> Result when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: state(),
    Result :: ok.
terminate(_Reason, _State) ->
    ok.

%% @private

-spec code_change(OldVersion, State, Extra) -> Result when
	OldVersion :: (term() | {down, term()}),
	State :: state(),
	Extra :: term(),
	Result :: {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reason({error, Reason}) -> Reason;
reason(Reason) -> Reason.
