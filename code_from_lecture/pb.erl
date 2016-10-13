%% Basic concurrent phone-book server callback module
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(pb).
-compile([export_all]).
-import(basicserver, [start/2, blocking/2]).
-behaviour(basicserver).



%% Interface
% We assume that the service will be started under the name phonebook
start()         -> start(phonebook, pb).
add(Contact)    -> blocking(phonebook, {add, Contact}).
list_all()      -> blocking(phonebook, list_all).
update(Contact) -> blocking(phonebook, {update, Contact}).


%% Callback functions
init() -> dict:new().
handle({add, {Name, _, _} = Contact}, Contacts) ->
    case dict:is_key(Name, Contacts) of 
        false -> {ok, dict:store(Name, Contact, Contacts)};
        true  -> {{error, {Name, is_already_there}},
                  Contacts}
    end;
handle(list_all, Contacts) ->
    List = dict:to_list(Contacts),
    {{ok, lists:map(fun({_, C}) -> C end, List)},
     Contacts};
handle({update, {Name, _, _} = Contact}, Contacts) ->
    {ok, dict:store(Name, Contact, Contacts)}.


