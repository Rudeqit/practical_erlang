-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms_pids = maps:new() :: map(),
                pids_rooms = maps:new() :: map()
        }).

%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(RoomName) ->
    gen_server:call(?MODULE, {create_room, RoomName}).

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).

get_users(RoomPid) ->
    gen_server:call(?MODULE, {get_users, RoomPid}).

send_message(RoomPid, UserName, Message) ->
    gen_server:call(?MODULE, {send_message, RoomPid, UserName, Message}).

get_history(RoomPid) ->
    gen_server:call(?MODULE, {get_history, RoomPid}).


%%% gen server API

init([]) ->
    {ok, #state{}}.

handle_call({create_room, RoomName}, _From, State) ->
    create_room_inner(RoomName, State);

handle_call(get_rooms, _From, State) ->
    Rooms = get_rooms_inner(State),
    {reply, Rooms, State};

handle_call({add_user, RoomPid, UserName, UserPid}, _From, State) ->
    add_user_inner(RoomPid, UserName, UserPid, State);

handle_call({remove_user, RoomPid, UserPid}, _From, State) ->
    remove_user_inner(RoomPid, UserPid, State);

handle_call({get_users, RoomPid}, _From, State) ->
    get_users_inner(RoomPid, State);

handle_call({send_message, RoomPid, UserName, Message}, _From, State) ->
    send_message(RoomPid, UserName, Message, State);

handle_call({get_history, RoomPid}, _From, State) ->
    get_history_inner(RoomPid, State).


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Ruquest, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%% inner function

create_room_inner(RoomName, State) ->
    {ok, RoomPid} = chat_room:start_link(),
    Rooms = State#state.rooms_pids,
    Pids = State#state.pids_rooms,
    NewState = State#state{rooms_pids = maps:put(RoomName, RoomPid, Rooms), 
                pids_rooms = maps:put(RoomPid, RoomName, Pids)},
    {reply, {RoomName, RoomPid}, NewState}.

get_rooms_inner(State) ->
    F = fun(RoomName, RoomPid, Acc) -> [{RoomName, RoomPid} | Acc] end,
    maps:fold(F, [], State#state.rooms_pids).

add_user_inner(RoomPid, UserName, UserPid, State) ->
    case maps:find(RoomPid, State#state.pids_rooms) of 
        {ok, _} ->
            Answer = chat_room:add_user(RoomPid, UserName, UserPid),
            {reply, Answer, State};
        error ->
            {reply, {error, room_not_found}, State}
    end.

remove_user_inner(RoomPid, UserPid, State) ->
    case maps:find(RoomPid, State#state.pids_rooms) of 
        {ok, _} ->
            Answer = chat_room:remove_user(RoomPid, UserPid),
            {reply, Answer, State};
        error ->
            {reply, {error, room_not_found}, State}
    end.

get_users_inner(RoomPid, State) ->
    case maps:find(RoomPid, State#state.pids_rooms) of 
        {ok, _} ->
            UsersList = chat_room:get_users(RoomPid),
            {reply, {ok, UsersList}, State};
        error ->
            {reply, {error, room_not_found}, State}
    end.

send_message(RoomPid, UserName, Message, State) ->
    case maps:find(RoomPid, State#state.pids_rooms) of 
        {ok, _} ->
            Answer = chat_room:add_message(RoomPid, UserName, Message),
            {reply, Answer, State};
        error ->
            {reply, {error, room_not_found}, State}
    end.

get_history_inner(RoomPid, State) ->
    case maps:find(RoomPid, State#state.pids_rooms) of 
        {ok, _} ->
            Messages = chat_room:get_history(RoomPid),
            {reply, {ok, Messages}, State};
        error ->
            {reply, {error, room_not_found}, State}
    end.
