%% Simple generic server library with transaction functionality
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2015
-module(trans_server).
-export([start/2, blocking/2]).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

blocking(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, {throw, Why}} -> throw(Why);
        {Pid, Reply} -> Reply
    end.

loop(Name, Mod, State) ->
    receive
        {From, Request} ->
            try Mod:handle(Request, State) of
                {Reply, State1} ->
                    From ! {Name, Reply},
                    loop(Name, Mod, State1)
            catch
                throw : Why ->
                    From ! {Name, {throw, Why}},
                    loop(Name, Mod, State)
            end
    end.
