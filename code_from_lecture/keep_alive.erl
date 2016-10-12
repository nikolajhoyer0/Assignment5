%% Concurrent phone-book server with simple supervisor functionality
%% 
-module(keep_alive).
-export([start/0, add/2, list_all/1, update/2]).

%% Interface

start() -> keep_looping().

add(Pid, Contact) ->
    blocking(Pid, {add, Contact}).

list_all(Pid) ->
    blocking(Pid, list_all).

update(Pid, Contact) ->
    blocking(Pid, {update, Contact}).


%% Internal implementation

blocking(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Response} -> Response
    end.

keep_looping() ->
    spawn(fun () ->
                  State = dict:new(),
                  process_flag(trap_exit, true),
                  Super = self(),
                  Worker = spawn_link(fun() -> loop(Super, State) end),
                  supervisor(Worker, none, State)
          end).

supervisor(Worker, LastMsg, LastGoodState) ->
    receive
        {'EXIT', Worker, Reason} ->
            io:format("~p exited because of ~p~n", [Worker, Reason]),
            Me = self(),
            NewWorker = spawn_link(fun() -> loop(Me, LastGoodState) end),
            case LastMsg of
                {Client, Ref, _Req} ->
                    Client ! {Ref, {you_killed_kenny_bastard, Reason}};
                _ -> do_nothing
            end,
            supervisor(NewWorker, none, LastGoodState);
        {backup_this, State} ->
            supervisor(Worker, LastMsg, State);
        Msg -> 
            Worker ! Msg,
            supervisor(Worker, Msg, LastGoodState)
    end.

loop(Supervisor, Contacts) ->
    Supervisor ! {backup_this, Contacts}, 
    receive
        {From, Ref, {add, {Name,_,_} = Contact}} ->
            case dict:is_key(Name, Contacts) of 
                false ->
                    From ! {Ref, ok},
                    loop(Supervisor, dict:store(Name, Contact, Contacts))
            end;
        {From, Ref, list_all} ->
            List = dict:to_list(Contacts),
            From ! {Ref, {ok, lists:map(fun({_, C}) -> C end, List)}},
            loop(Supervisor, Contacts);
        {From, Ref, {update, {Name,_,_} = Contact}} ->
            From ! {Ref, ok},
            loop(Supervisor, dict:store(Name, Contact, Contacts))
    end.
