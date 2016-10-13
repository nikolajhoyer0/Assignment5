-module(worker).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3]).

%%% API
start(Code) ->
    gen_statem:start({local, mr}, mr, [], []).

job(Pid, NWorkers, MapFun, RedInput, Initial, Data) ->
    gen_statem:cast(mr, {button, Digit}).

stop(Pid) ->
    gen_statem:stop(mr).

%%% Callback
init(Code) ->
    {ok, locked, {[], Code}}.

callback_mode() ->
    state_functions.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

terminate(normal, _StateName, _StateData) ->
    void.

%%% State
locked(cast, {button, Digit}, {SoFar, Code}) ->
    case [Digit|SoFar] of
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


open(timeout, _, State) ->
    do_lock(),
    {next_state, locked, State}.
