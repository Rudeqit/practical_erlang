-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% -export([add_message_inner/3, get_messages_inner/1]).

-type(name() :: binary()).
-type(message() :: {name(), binary()}). 

-record(state, {
          history = [] :: [message()]
        }).

%%% module API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add_message(Pid, Name, Message) ->
    gen_server:call(Pid, {add_message, Name, Message}).

get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

%%% gen server API

init([]) ->
    {ok, #state{}}.

handle_call({add_message, Name, Message}, _From, State) ->
    NewState = add_message_inner(Name, Message, State),
    {reply, ok, NewState};

handle_call(get_messages, _From, State) ->
    get_messages_inner(State).

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%% inner function

add_message_inner(Name, Message, State) ->
    History = State#state.history, 
    State#state{history = [{Name, Message} | History]}.

get_messages_inner(State) ->
    History = State#state.history,
    ChronologyHist = lists:reverse(History), 
    {reply, ChronologyHist, State}.