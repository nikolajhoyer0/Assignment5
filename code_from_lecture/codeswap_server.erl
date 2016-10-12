%% Simple generic server library with hot-swap code functionality
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2015
-module(codeswap_server).
-export([start/2, blocking/2, swap_code/2]).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) -> blocking(Name, {swap_code, Mod}).

blocking(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Reply} -> Reply
    end.

loop(Name, Mod, State) ->
    receive
        {From, {swap_code, NewMod}} ->
            From ! {Name, ok},
            loop(Name, NewMod, State);
        {From, Request} ->
            {Reply, State1} = Mod:handle(Request, State),
            From ! {Name, Reply},
            loop(Name, Mod, State1)
    end.
