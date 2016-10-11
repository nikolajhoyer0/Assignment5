-module(mr).
-export([start/0,
         job/6,
         stop/1]).

start() -> ok.

%%%
%%% MapFun  :: fun(A -> B)
%%% RedFun  :: {fun((B, Res) -> Res), Mode}
%%% Initial :: Res
%%% Data    :: [A]
%%% Mode    :: single | multi
%%%
job(Pid, NWorkers, MapFun, RedFun, Initial, Data) -> ok.

stop(Pid) -> ok.
