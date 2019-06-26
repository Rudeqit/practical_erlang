-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    Parts = binary:split(Str, <<"{{">>, [global]),
    Parts2 = 
        lists:map(fun(Part)->
                    case binary:split(Part, <<"}}">>) of
                        [PartWithoutParam] -> PartWithoutParam;
                        [Param | Rest] ->
                            case maps:find(Param, Data) of
                                error -> Rest;
                                {ok, Templ} when is_binary(Templ) -> 
                                    [Templ | Rest];
                                {ok, Templ} when is_integer(Templ) ->
                                    [integer_to_binary(Templ) | Rest];
                                {ok, Templ} when is_list(Templ) ->
                                    [list_to_binary(Templ) | Rest]
                            end
                    end
                  end, Parts),
    unicode:characters_to_binary(Parts2).

    