-module(prop_buffalo).
-include_lib("proper/include/proper.hrl").

-export([send/2,send_sleep/2]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_timeout() ->
    ?SETUP(setup(),
    ?FORALL(Unit, range(100,200),
    begin
		Msg = ping,
		buffalo:queue({?MODULE, send, [self(), Msg]}, #{ timeout => Unit}),
		ok = dont_receive(Unit-10),
		Msg == do_receive(Msg, Unit)
    end)
    ).

prop_cancel() ->
    ?SETUP(setup(),
    ?FORALL(Unit, range(100,200),
    begin
		Msg = ping,
		buffalo:queue(cancel_test, {?MODULE, send, [self(), Msg]}, #{ timeout => Unit }),
		buffalo:cancel(cancel_test),
		ok == dont_receive(2*Unit)
    end)
    ).

prop_update() ->
    ?SETUP(setup(),
    ?FORALL(Unit, range(100,200),
    begin
		Msg = ping,
		buffalo:queue({?MODULE, send, [self(), Msg]}, #{ timeout => Unit }),
		ok = dont_receive(trunc(0.7*Unit)),
		buffalo:queue({?MODULE, send, [self(), Msg]}, #{ timeout => Unit }),
		ok = dont_receive(trunc(0.7*Unit)),
		Msg == do_receive(Msg, Unit)
    end)
    ).

%% This scenario is not stable. 
% prop_drop_if_running() ->
%     ?SETUP(setup(),
%     ?FORALL(_Unit, range(100,200),
%     begin
% 		Sleep = 100,
% 		Key = test_drop_if_running,
% 		MFA = {?MODULE, send_sleep, [self(), Sleep]},
% 		{ok, new} = buffalo:queue(Key, MFA, #{ timeout => 0 }),
% 		ok = receive
% 			start -> ok
% 		after
% 			20 -> timeout
% 		end,
% 		{ok, running} = buffalo:status(Key),
% 		{ok, running} = buffalo:queue(Key, MFA, #{ is_drop_running => true, timeout => 0 }),
% 		ok = receive
% 			stop -> ok
% 		after
% 			200 -> timeout
% 		end,
% 		{error, notfound} = buffalo:status(Key),
% 		{ok, new} = buffalo:queue(Key, MFA, #{ timeout => 0 }),
% 		ok = receive
% 			start -> ok
% 		after
% 			20 -> timeout
% 		end,
% 		ok = receive
% 			stop -> ok
% 		after
% 			200 -> timeout
% 		end,
		
% 		true
%     end)
%     ).

prop_deadline() ->
	?FORALL(Unit, range(100,100),
	begin
		ok = application:start(buffalo),
		
		Msg = ping,
	% 	MFA = {?MODULE, send, [self(), Msg]},
	% 	Key = test_deadline,
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	{ok, queued} = buffalo:status(Key),
	% 	ok = dont_receive(trunc(0.7*Unit)),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	ok = dont_receive(trunc(0.7*Unit)),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	ok = dont_receive(trunc(0.7*Unit)),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	ok = dont_receive(trunc(0.7*Unit)),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	ok = dont_receive(trunc(0.7*Unit)),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	ok = dont_receive(trunc(0.7*Unit)),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	ok = dont_receive(trunc(0.2*Unit)),
	% 	%Result = dont_receive(trunc(0.2*Unit)),
	% 	%io:format("Result = ~p~n",[Result]),
	% 	buffalo:queue(Key, MFA, #{ timeout => Unit }),
	% 	% We are now at 4.7 x initial unit
	% 	% The deadline (at 5x) should trigger soon.
	% 	Msg = do_receive(Msg, trunc(0.4*Unit)),
	% 	% And the queue should be emptied.
	% 	ok = dont_receive(trunc(1.3*Unit)),
		
	% 	application:stop(buffalo),
	% 	application:unload(buffalo),
	% 	true
	% end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
setup() ->
    fun() ->
        ok = application:start(buffalo),
        fun() -> 
			application:stop(buffalo)
		end
    end.

dont_receive(Time) ->
    receive
        Msg ->
            {error, Msg}
    after
        Time -> ok
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
	
%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

