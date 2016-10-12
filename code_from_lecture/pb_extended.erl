%% Basic concurrent phone-book server callback module with extended interface
%% 
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Date: October, 2015
-module(pb_extended).
-compile([export_all]).
-import(codeswap_server, [blocking/2]).

%% Interface
add(Contact)    -> blocking(phonebook, {add, Contact}).
list_all()      -> blocking(phonebook, list_all).
update(Contact) -> blocking(phonebook, {update, Contact}).

delete(Name) -> blocking(phonebook, {delete, Name}).
find(Name) -> blocking(phonebook, {find, Name}).


%% Callback functions
init() -> dict:new().
handle({delete, Name}, Contacts) ->
    {ok, dict:erase(Name, Contacts)};
handle({find, Name}, Contacts) ->
    {dict:find(Name, Contacts), Contacts};
handle({add, {Name, _, _} = Contact}, Contacts) ->
    case dict:is_key(Name, Contacts) of 
        false -> {ok, dict:store(Name, Contact, Contacts)};
        true  -> {{error, Name, is_already_there},
                  Contacts}
    end;
handle(list_all, Contacts) ->
%    io:format("New module ~n"),
    List = dict:to_list(Contacts),
    {{ok, lists:map(fun({_, C}) -> C end, List)},
     Contacts};
handle({update, {Name, _, _} = Contact}, Contacts) ->
    {ok, dict:store(Name, Contact, Contacts)}.


