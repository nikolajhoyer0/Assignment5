-module(master).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3]).

%%% API
start() ->
    gen_statem:start({local, master}, master, [], []).

job(Pid, NWorkers, MapFun, RedInput, Initial, Data) ->
    Msg = {job, Pid, NWorkers, MapFun, RedInput, Initial, Data},
    gen_statem:call(master, Msg).

stop(Pid) ->
    gen_statem:stop(master).

%%% Callback
init() ->
    {ok, map, none}.

callback_mode() ->
    state_functions.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

terminate(normal, _StateName, _StateData) ->
    void.

%%% State functions
idle() ->
    ok.

map(call, {job, Pid, NWorkers, MapFun, RedInput, Initial, Data}, State) ->
    do_map(State),
    {keep_state, State};
map(part_svar, X, State) ->
    NewState = {Pid, NWorkers, MapFun, RedInput, Initial, Data},
    case [X|SoFar] of
        Code ->
            do_unlock(),
            {next_state, open, {[], Code}, 5000};
        Incomplete when length(Incomplete)<length(Code) ->
            beep(),
            {next_state, locked, {Incomplete, Code}};
        _Wrong ->
            thats_not_gonna_do_it(),
            {keep_state, {[], Code}}
      end.
    {next_state, reduce, NewState}.
map(final, X, State) ->

reduce(timeout, _, State) ->
    do_reduce(State),
    {next_state, idle, none}.

%%% Internal stuff
do_map(State) ->
    {Pid, NWorkers, MapFun, _, _, Data} = State,
    spawn(),
    % Spawn n workers
    ok.

do_reduce(State) ->
    {Pid, NWorkers, _, RedInput, Initial, Data} = State,
    ok.
