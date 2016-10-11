-module(mr).
-export([start/0,
         job/6,
         stop/1]).

start() -> ok.

job(Pid, NWorkers, MapFun, RedFun, Initial, Data) -> ok.

stop(Pid) -> ok.
