-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% -export([add_user_inner/3, remove_user_inner/2, add_message_inner/3, get_users_inner/1]).

-type(name() :: binary()).
% -type(user() :: pid()).
-type(message() :: {name(), binary()}).

-record(state, {
          users_pids = maps:new() :: map(), %% Key(User_Name) -> Value(chat_user Pid)
          pids_users = maps:new() :: map(), %% Key(chat_user Pid) -> Value(User_Name)
          history = [] :: [message()]
        }).

%%% module API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add_user(ServerPid, Name, UserPid) ->
    gen_server:call(ServerPid, {add_user, Name, UserPid}).

remove_user(Pid, UserPid) ->
    gen_server:call(Pid, {remove_user, UserPid}).

get_users(Pid) ->
    gen_server:call(Pid, get_users).

add_message(Pid, Name, Message) ->
    gen_server:call(Pid, {add_message, Name, Message}).

get_history(Pid) ->
    gen_server:call(Pid, get_history).

%%% gen server API

init([]) ->
    {ok, #state{}}.

handle_call({add_user, Name, UserPid}, _From, State) ->
    NewState = add_user_inner(Name, UserPid, State),
    {reply, ok, NewState};

handle_call({remove_user, UserPid}, _From, State) ->
    remove_user_inner(UserPid, State);

handle_call(get_users, _From, State) -> 
    Users = get_users_inner(State),
    {reply, Users, State};

handle_call({add_message, Name, Message}, _From, State) ->
    NewState = add_message_inner(Name, Message, State),
    {reply, ok, NewState};

handle_call(get_history, _From, State) ->
    History = State#state.history,
    ChronologyHist = lists:reverse(History), 
    {reply, ChronologyHist, State}.

handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%% inner function

add_user_inner(Name, UserPid, State) ->
    New_Users_Pids = maps:put(Name, UserPid, State#state.users_pids),
    New_Pids_Users = maps:put(UserPid, Name, State#state.pids_users),
    State#state{users_pids = New_Users_Pids, pids_users = New_Pids_Users}.

remove_user_inner(UserPid, State) ->
    case maps:find(UserPid, State#state.pids_users) of
        {ok, Name} ->
            Users_Pids = State#state.users_pids,
            Pids_Users = State#state.pids_users,
            NewState = State#state{users_pids = maps:remove(Name, Users_Pids), 
                                   pids_users = maps:remove(UserPid, Pids_Users)},
            {reply, ok, NewState};
        error ->
            {reply, {error, user_not_found}, State}
    end. 

add_message_inner(Name, Message, State) ->
    F = fun(Pid) -> 
            chat_user:add_message(Pid, Name, Message) end,
    UsersPids = maps:values(State#state.users_pids),
    lists:foreach(F, UsersPids),
    History = State#state.history,
    State#state{history = [{Name, Message} | History]}.

get_users_inner(State) ->
    F = fun(Key, Value, Acc) -> [{Key, Value} | Acc] end,
    maps:fold(F, [], State#state.users_pids).
