-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.

win(GameState) ->
	Win_x = win_combinations(GameState, x),
	Win_o = win_combinations(GameState, o),
	if
		Win_x -> {win, x};
		Win_o -> {win, o};
		true -> no_win 
	end.
	
win_combinations(GameState, S) ->
	case GameState of      
	    {{S,S,S},
	     {_,_,_},
	     {_,_,_}} -> true;

	    {{_,_,_},
	     {S,S,S},
	     {_,_,_}} -> true;

	    {{_,_,_},
	     {_,_,_},
	     {S,S,S}} -> true;

	    {{S,_,_},
	     {S,_,_},
	     {S,_,_}} -> true;

	    {{_,S,_},
	     {_,S,_},
	     {_,S,_}} -> true;

	    {{_,_,S},
	     {_,_,S},
	     {_,_,S}} -> true;

	    {{S,_,_},
	     {_,S,_},
	     {_,_,S}} -> true;

	    {{_,_,S},
	     {_,S,_},
	     {S,_,_}} -> true;

	     _ -> false
	end.

%% {{1, 2, 3},{4, 5, 6},{7, 8, 9}}
move(Cell, Player, GameState) ->
	Valid = valid_move(Cell, GameState),
	case GameState of
		{_, _, _} when Valid == false -> {error, invalid_move}; 
		{S1, S2, S3} when Cell =< 3 -> 
			{ok, {set(Cell, Player, S1), S2, S3}};
		{S1, S2, S3} when Cell > 3, Cell =< 6 -> 
			{ok, {S1, set(Cell - 3, Player, S2), S3}};
		{S1, S2, S3} when Cell > 6 ->
			{ok, {S1, S2, set(Cell - 6, Player, S3)}}
	end.


valid_move(Cell, {S1, S2, S3}) ->
	Elem = if
		Cell =< 3 -> element(Cell, S1);
		Cell > 3, Cell =< 6 -> element(Cell - 3, S2);
		Cell > 6, Cell =< 9 -> element(Cell - 6, S3);
		true -> false
	end,
	case Elem of
		f -> true;
		_ -> false
	end.

set(1, Player, {S1, S2, S3}) -> {Player, S2, S3};
set(2, Player, {S1, S2, S3}) -> {S1, Player, S3};
set(3, Player, {S1, S2, S3}) -> {S1, S2, Player}.
	