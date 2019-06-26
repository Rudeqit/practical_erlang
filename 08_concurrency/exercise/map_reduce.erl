-module(map_reduce).

%% -export([start/1, receiver/2]).
-compile(export_all).

start(Files) ->
    RootPid = self(),
    NumWorker = length(Files),
    ReceiverPid = spawn(?MODULE, receiver, [RootPid, NumWorker]),
    [spawn(?MODULE, mapper, [ReceiverPid, File]) || File <- Files],
    receive
        {_, Data} -> Data
    after
        2000 -> {error, no_reply}
    end.   

receiver(RootPid, NumWorker) ->
    DataList = [wait_data() || _ <- lists:seq(1, NumWorker)],
    Result = reduce(DataList),
    RootPid ! {self(), Result}.

wait_data() ->
    receive
        {_, _, _, Data} ->
            Data
    after 
        2000 -> io:format("Wait data have no receive file"),
        #{}
    end. 


reduce(DataList) ->
    Acc = maps:new(),
    Result = reduce(DataList, Acc),
    Result.

reduce([], Result) -> Result;
reduce([Data | Tail], Result) ->    
    Keys = maps:keys(Data),
    Result1 = check_keys(Keys, Data, Result),
    reduce(Tail, Result1).

mapper(ParentPid, File) ->
    Data = read_file(File),
    ParentPid ! {mapper, self(), File, Data}.

read_file(File) ->
    case file:read_file(File) of
        {ok, BinaryFile} -> analyze_file(BinaryFile);
        {error, _} -> #{}
    end.

analyze_file(BinaryFile) -> 
    Words = binary:split(BinaryFile, [<<" ">>, <<"\n">>], [global]),
    lists:foldl(fun
                (<<>>, Acc) -> Acc; 
                (Word, Acc) ->
                    case maps:find(Word, Acc) of    
                        {ok, Counter} -> Acc#{Word => Counter + 1};
                        error -> Acc#{Word => 1}
                    end
                end, #{}, Words).



check_keys([], _, Data) -> Data;
check_keys([Key | Tail], DataReceive, Data) ->
    case maps:is_key(Key, Data) of
        true ->
            Value = maps:get(Key, Data) + maps:get(Key, DataReceive), 
            Data2 = maps:update(Key, Value, Data),
            check_keys(Tail, DataReceive, Data2);
        false ->
            Value = maps:get(Key, DataReceive),
            Data2 = maps:put(Key, Value, Data),
            check_keys(Tail, DataReceive, Data2)
    end.

% read_file(PPid, File) ->
%     Data = maps:new(),
%     case file:open(File, read) of
%         {ok, IoDevice} ->  
%             Data2 = read_line(IoDevice, Data),
%             PPid ! {self(), File, Data2};
%         _ ->
%             PPid ! {self(), File, Data}
%     end.

% read_line(IoDevice, Data) ->
%     case file:read_line(IoDevice) of 
%         {ok, Str} ->
%             BinStr = unicode:characters_to_binary(Str),
%             BinWords = binary:split(BinStr, [<<" ">>], [global]),
%             Data2 = update_map(BinWords, Data),
%             read_line(IoDevice, Data2);
%         eof -> 
%             Data;
%         {error, Reason} -> 
%             {error, Reason}
%     end.

% update_map([], Data) -> Data;    
% update_map([BinWord | Tail], Data) ->
%     Data2 = maps:update_with(BinWord, fun(V) -> V + 1 end, 1, Data),
%     update_map(Tail, Data2).
