-module(vars).
-export([exec/2]).

exec(tm_command, Iter) ->
    tm_menu:get_selection(list(Iter)).

list(Iter) ->
    sets:to_list(
        sets:from_list(
            list(Iter, Iter()))).

list(_, eof) ->
    [];
list(Iter, Line) ->
    matches(Line, list(Iter, Iter())).

matches(Line, MatchesIn) ->
    {match, MatchIndices} = regexp:matches(Line, "[A-Z][a-zA-Z0-9_@]*"),
    lists:foldl(
        fun({Start, Length}, Matches) ->
            [string:substr(Line, Start, Length)|Matches]
        end,
        MatchesIn, MatchIndices).
