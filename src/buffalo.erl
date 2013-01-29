-module(buffalo).
-export([start/0, queue/4, cancel/3]).

%% API

start() ->
    application:start(buffalo).

queue(Module, Function, Arguments, Timeout) ->
    buffalo_queuer:queue(Module, Function, Arguments, Timeout).

cancel(Module, Function, Arguments) ->
    buffalo_queuer:cancel(Module, Function, Arguments).
