-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsp, {A,B,C}),
    dict:new().


create_short(LongLink, State) ->
    Link = state_filter(dict:to_list(State), LongLink),      
    case Link of
        {ok, ShortLink} -> 
            {ShortLink, State};
        error ->
            ShortLinkCreated = string:concat("http://hexlet.io/", rand_str(length(LongLink))),
            StateUpd = dict:store(ShortLinkCreated, LongLink, State),
            {ShortLinkCreated, StateUpd}
    end.

state_filter([{ShortLink, LongLink} | _], LongLink) -> 
    {ok, ShortLink};
state_filter([_ | Tail], LongLink) ->
    state_filter(Tail, LongLink);
state_filter([], _) ->
    error.


get_long(ShortLink, State) ->
    case dict:find(ShortLink, State) of
        {ok, LongLink} ->
            {ok, LongLink};
        error -> 
            {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
