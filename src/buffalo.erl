-module(buffalo).
-export([start/0]).

%% API

start() ->
    application:start(buffalo).
