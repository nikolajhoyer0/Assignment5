%% Meant to be copy-pasted to an eshell.
%% 

pb:start().

pb:add({"Ken", "Volosvej", "21424"}).

L = [{"Oleks", "Kantinen", "21423"},
     {"Donald Duck", "Duckville", "555-4242"},
     {"Ken", "Universitetsparken 5", "21424"}].

lists:map(fun pb:add/1, L).


%% % Even though pb is compiled against basicserver it works with
%% % codeswap_server, because blocking is defined the same way in both modules.
codeswap_server:start(phonebook, pb). 


%% % Now let's switch to pb_extended
%% codeswap_server:swap_code(phonebook, pb_extended).

%% % We can use the new API
%% pb_extended:delete("Ken").

%% % ... or the old
%% pb_extended:list_all().

%% % ... we can even continue to use the old pb module
%% pb:list_all().
