-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2,
         loop/1, call/2, handle_call/2, search_room/2, search_user/3]).

-record(room, {ref,
               name = "",
               users = [],
               msgs = []
            }).

start() ->
    io:format("Start nandmade gen server~n"),
    InitState = [],
    spawn(?MODULE, loop, [InitState]).

create_room(Server, Room) ->
    call(Server, {create_room, Room}).

remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).

get_rooms(Server) ->
    call(Server, get_rooms).

add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).

remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).

get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).

send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).

get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call, Ref, self(), Msg},
    receive
        {reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            Reason
    after 5000 ->
        erlang:demonitor(Ref, [flush]),
        noreply
    end.

loop(State) ->
    io:format("Start infinite loop~n"),
    receive
        {call, Ref, From, Msg} -> 
            {Reply, NewState} = ?MODULE:handle_call(Msg, State),
            From ! {reply, Ref, Reply},
            ?MODULE:loop(NewState);
        stop ->
            ok;
        Msg ->
            io:format("Captain, we have unknown message type: ~p~n", [Msg]),
            ?MODULE:loop(State)
    end.


handle_call({create_room, _}, State) when erlang:length(State) > 4 ->
    {{error, room_limit}, State};

handle_call({create_room, Room}, State) ->
    NewRoom = #room{ref = erlang:make_ref(), name = Room, users = [], msgs = []},
    NewState = [NewRoom | State],
    io:format("Room ~p add~n", [Room]),
    {{ok, NewRoom#room.ref}, NewState};

handle_call(get_rooms, State) ->
    Reply = lists:foldl(fun(Room, RoomNames) -> [{Room#room.ref, Room#room.name} | RoomNames] end, [], State),
    {Reply, State};

handle_call({remove_room, RefRemoveRoom}, State) ->
    case search_room(RefRemoveRoom, State) of
        {ok, FindRoom} -> 
            NewState = lists:delete(FindRoom, State),
            io:format("Room ~p removed~n", [FindRoom#room.name]),
            {ok, NewState};
        Msg -> 
            Msg
    end;

handle_call({add_user, RefRoom, UserName}, State) ->
    F = fun(CurrentRoom) -> 
        case CurrentRoom#room.ref =:= RefRoom of
            true ->
                TailUsers = CurrentRoom#room.users,
                CurrentRoom#room{users = [UserName | TailUsers]};
            false -> CurrentRoom
        end
    end,
    case search_user(RefRoom, UserName, State) of
        {not_found, _} -> 
            NewState = lists:map(F, State),
            io:format("User ~p add~n", [UserName]),
            {ok, NewState};
        {found, _} ->
            {{error, user_is_in_room}, State} ;
        Msg -> 
            Msg
    end;

handle_call({remove_user, RefRoom, UserName}, State) ->
    F = fun(CurrentRoom) -> 
        case CurrentRoom#room.ref =:= RefRoom of
            true ->
                UsersList = CurrentRoom#room.users,
                NewUserList = lists:delete(UserName, UsersList),
                CurrentRoom#room{users = NewUserList};
            false -> CurrentRoom
        end
    end,
    case search_user(RefRoom, UserName, State) of
        {not_found, _} -> 
            {{error, user_not_in_room}, State};
        {found, _} ->
            NewState = lists:map(F, State),
            io:format("User ~p remove~n", [UserName]),
            {ok, NewState};
        Msg -> 
            Msg
    end;

handle_call({get_users_list, RefRoom}, State) ->
    case search_room(RefRoom, State) of
        {ok, FindRoom} -> 
            UsersList = FindRoom#room.users,
            {{ok, UsersList}, State};
        Msg -> 
            Msg
    end;

handle_call({send_message, RefRoom, UserName, Message}, State) ->
    F = fun(CurrentRoom) -> 
        case CurrentRoom#room.ref =:= RefRoom of
            true ->
                Messages = CurrentRoom#room.msgs,
                CurrentRoom#room{msgs = [{UserName, Message} | Messages]}; 
            false -> CurrentRoom
        end
    end,
    case search_user(RefRoom, UserName, State) of
        {not_found, _} -> 
            {{error, user_not_in_room}, State};
        {found, _} ->
            NewState = lists:map(F, State),
            io:format("Message have been send: {~p, ~p} remove~n", [UserName, Message]),
            {ok, NewState};
        Msg -> 
            Msg
    end;
handle_call({get_messages_history, RefRoom}, State) ->
    case search_room(RefRoom, State) of
        {ok, FindRoom} -> 
            Messages = FindRoom#room.msgs,
            {{ok, Messages}, State};
        Msg -> 
            Msg
    end.


search_room(RefRoom, State) ->
    F = fun(CurrentRoom) -> CurrentRoom#room.ref =:= RefRoom end,
    case lists:search(F, State) of
        {value, FindRoom} ->
            {ok, FindRoom};
        false -> {{error, room_not_found}, State}
    end.

search_user(RefRoom, UserName, State) ->
    case search_room(RefRoom, State) of
        {ok, FindRoom} -> 
            case lists:member(UserName, FindRoom#room.users) of
                true ->
                    {found, FindRoom};
                false ->
                    {not_found, FindRoom}
            end;
        Msg ->
            Msg
    end.
