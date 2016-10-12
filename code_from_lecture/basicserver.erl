%% Simple generic server library
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2016

-module(basicserver).
-export([start/2, blocking/2]).

-callback init() -> State :: term().
-callback handle(Arg :: term(), State :: term()) ->
    { ok, State :: term() } |
    { {error, Reason :: term()}, State :: term() }.
        


start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

blocking(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, Reply} -> Reply
    end.

loop(Name, Mod, State) ->
    receive
        {From, Request} ->
            {Reply, State1} = Mod:handle(Request, State),
            From ! {Name, Reply},
            loop(Name, Mod, State1)
    end.
