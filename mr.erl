-module(mr).
-export([start/0,
         job/6,
         stop/1,
         simple_test/0]).

start() -> {ok, self()}.

%%%
%%% MapFun  :: fun(A -> B)
%%% RedFun  :: {fun((B, Res) -> Res), Mode}
%%% Initial :: Res
%%% Data    :: [A]
%%% Mode    :: single | multi
%%%
job(Pid, NWorkers, MapFun, RedInput, Initial, Data) ->
    {RedFun, Method} = RedInput,
    Map = lists:map(MapFun, Data),
    Reduce = lists:foldl(RedFun, Initial, Map),
    {ok, Reduce}.

stop(Pid) -> ok.
