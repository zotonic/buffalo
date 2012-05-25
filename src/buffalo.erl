-module(buffalo).
-export([start/0, queue/4]).

%% API

start() ->
    application:start(buffalo).

queue(Module, Function, Arguments, Timeout) ->
    buffalo_queuer:queue(Module, Function, Arguments, Timeout).
