-module(funs).
-export([exec/2]).

exec(tm_command, Iter) ->
    {ok, [Name, ArityRaw]} = regexp:split(
        tm_menu:get_selection(menu_items(Iter)), $/),
    {Arity, _} = string:to_integer(ArityRaw),
    snippet({Name, Arity}).

menu_items(Iter) ->
    Module = list_to_atom(Iter()),
    lists:map(
        fun({Name, Arity}) ->
            io_lib:format("~s/~B", [Name, Arity])
        end,
        Module:module_info(exports)).

snippet({Name, 0}) ->
    io_lib:format("~s()", [Name]);
snippet({Name, Arity}) ->
    io:format("Name=~p Arity=~p~n", [Name, Arity]),
    io_lib:format(lists:flatten(["~s("|
        lists:duplicate(Arity-1, "${~B:Arg}, ")], "${~B:Arg})"),
        [Name|lists:seq(1,Arity)]).
