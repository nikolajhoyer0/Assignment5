%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <ken@friislarsen.net>
%%% @copyright (C) 2016, Ken Friis Larsen
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2016 by Ken Friis Larsen <ken@friislarsen.net>
%%%-------------------------------------------------------------------
-module(pb_gs).

-behaviour(gen_server).

%% API
-export([start_link/0, add/1, list_all/0, update/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

add(Contact)    -> gen_server:call(?MODULE, {add, Contact}).
list_all()      -> gen_server:call(?MODULE, list_all).
update(Contact) -> gen_server:call(?MODULE, {update, Contact}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, {Name, _, _} = Contact}, _From, Contacts) ->
    case dict:is_key(Name, Contacts) of 
        false -> {reply, ok, dict:store(Name, Contact, Contacts)};
        true  -> {reply, {error,Name, is_already_there}, Contacts}
    end;
handle_call(list_all, _From, Contacts) ->
    List = dict:to_list(Contacts),
    {reply, {ok, lists:map(fun({_, C}) -> C end, List)},
     Contacts};
handle_call({update, {Name, _, _} = Contact}, _From, Contacts) ->
    {reply, ok, dict:store(Name, Contact, Contacts)};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
