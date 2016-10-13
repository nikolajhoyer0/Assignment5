-module(mr).
-export([start/0, job/6, stop/1]).

%%% Entry point for the application
start() ->
    master:start().

job(Pid, NWorkers, MapFun, RedInput, Initial, Data) ->
    master:job(Pid, NWorkers, MapFun, RedInput, Initial, Data).

stop(Pid) ->
    master:stop(Pid).
