%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright 2013-2019 Paul Peregud
%% @doc Tests for buffalo

%% Copyright 2013-2019 Paul Peregud
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

-module(buffalo_test).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    send/2,
    send_sleep/2
    ]).

%% tests

all_test_() ->
    {foreach, fun () -> setup() end,
     fun (State) -> cleanup(State) end,
     [{timeout, 100, fun test_timeout/0}
      , {timeout, 100, fun test_update/0}
      , {timeout, 100, fun test_deadline/0}
      , {timeout, 100, fun test_cancel/0}
      , {timeout, 100, fun test_drop_if_running/0}
      ]}.

setup() ->
    application:start(buffalo).

cleanup(_) ->
    application:stop(buffalo).

test_timeout() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue({?MODULE, send, [self(), Msg]}, #{ timeout => Unit}),
    ok = dont_receive(Unit-10),
    Msg = do_receive(Msg, Unit),
    ok.

test_cancel() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue(cancel_test, {?MODULE, send, [self(), Msg]}, #{ timeout => Unit }),
    buffalo:cancel(cancel_test),
    ok = dont_receive(2*Unit),
    ok.

test_update() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue({?MODULE, send, [self(), Msg]}, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue({?MODULE, send, [self(), Msg]}, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    Msg = do_receive(Msg, Unit),
    ok.

test_deadline() ->
    Unit = 100,
    Msg = ping,
    MFA = {?MODULE, send, [self(), Msg]},
    Key = test_deadline,
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    {ok, queued} = buffalo:status(Key),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    ok = dont_receive(trunc(0.5*Unit)),
    buffalo:queue(Key, MFA, #{ timeout => Unit }),
    % We are now at 4.7 x initial unit
    % The deadline (at 5x) should trigger soon.
    Msg = do_receive(Msg, trunc(0.4*Unit)),
    % And the queue should be emptied.
    ok = dont_receive(trunc(1.2*Unit)).

test_drop_if_running() ->
    Sleep = 100,
    Key = test_drop_if_running,
    MFA = {?MODULE, send_sleep, [self(), Sleep]},
    {ok, new} = buffalo:queue(Key, MFA, #{ timeout => 0 }),
    ok = receive
        start -> ok
    after
        20 -> timeout
    end,
    {ok, running} = buffalo:status(Key),
    {ok, running} = buffalo:queue(Key, MFA, #{ is_drop_running => true, timeout => 0 }),
    ok = receive
        stop -> ok
    after
        200 -> timeout
    end,
    {error, notfound} = buffalo:status(Key),
    {ok, new} = buffalo:queue(Key, MFA, #{ timeout => 0 }),
    ok = receive
        start -> ok
    after
        20 -> timeout
    end,
    ok = receive
        stop -> ok
    after
        200 -> timeout
    end,
    ok.

dont_receive(T) ->
    receive
        Msg ->
            {error, Msg}
    after
        T -> ok
    end.

do_receive(What, After) ->
    receive
        What ->
            What
    after After ->
            timeout
    end.

send(Pid, Msg) ->
    Pid ! Msg,
    ok.

send_sleep(Pid, Sleep) ->
    Pid ! start,
    timer:sleep(Sleep),
    Pid ! stop,
    ok.

