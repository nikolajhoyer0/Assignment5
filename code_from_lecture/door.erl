-module(door).
-behaviour(gen_statem).

-export([start/1, stop/0, button/1]).
-export([locked/3, open/3]).
-export([init/1, callback_mode/0, code_change/4, terminate/3]).


%%% Client API

start(Code) ->
    gen_statem:start({local, door}, door, lists:reverse(Code), []).

button(Digit) ->
    gen_statem:cast(door, {button, Digit}).

stop() ->
    gen_statem:stop(door).


%%% Callback functions

callback_mode() ->
    state_functions.

init(Code) ->
    do_lock(),
    {ok, locked, {[], Code}}.

terminate(normal, _StateName, _StateData) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

%%%% State functions

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
%% open(cast, {button, _Digit}, State) ->
%%     beep(),
%%     {next_state, open, State, 5000}.


%%% Internal stuff

do_lock() ->
    io:format("COMPUTER VOICE: Door is locked ~nKeep out~n").

do_unlock() ->
    io:format("COMPUTER VOICE: Door is unlocked ~nCome in~n").

thats_not_gonna_do_it() ->
    io:format("COMPUTER VOICE: Booo. You'll never get in~n").
    
beep() ->
    io:format("BEEP~n").
