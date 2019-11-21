%%%-------------------------------------------------------------------
%%% @author Paul Peregud <paulperegud@gmail.com>
%%% @copyright (C) 2013, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2013 by Paul Peregud <paulperegud@gmail.com>
%%% Modified : 30 Jan 2014 by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------
-module(buffalo_test).
 
-include_lib("eunit/include/eunit.hrl").
 
%% API
-export([
    send/2
    ]).
 
%% tests
 
all_test_() ->
    {foreach, fun () -> setup() end,
     fun (State) -> cleanup(State) end,
     [{timeout, 100, fun test_timeout/0}
      , {timeout, 100, fun test_update/0}
      , {timeout, 100, fun test_deadline/0}
      , {timeout, 100, fun test_cancel/0}
      ]}.
 
setup() ->
    application:start(buffalo).
 
cleanup(_) ->
    application:stop(buffalo).
 
test_timeout() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(Unit-10),
    Msg = do_receive(Msg, Unit),
    ok.

test_cancel() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue(cancel_test, ?MODULE, send, [self(), Msg], Unit),
    buffalo:cancel(cancel_test),
    ok = dont_receive(2*Unit),
    ok.
 
test_update() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    Msg = do_receive(Msg, Unit),
    ok.
 
test_deadline() ->
    Unit = 100,
    Msg = ping,
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.7*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    ok = dont_receive(trunc(0.5*Unit)),
    buffalo:queue(?MODULE, send, [self(), Msg], Unit),
    % We are now at 4.7 x initial unit
    % The deadline (at 5x) should trigger soon.
    Msg = do_receive(Msg, trunc(0.4*Unit)),
    % And the queue should be emptied.
    ok = dont_receive(trunc(1.2*Unit)).
 
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

