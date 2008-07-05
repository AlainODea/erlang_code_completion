-module (textmate_complete).
-export ([string/1]).
-compile(export_all).

string(String) -> complete(complete:string(String)).

complete({call, [Option]}) -> call(Option);
complete({call, Options}) -> call(calls(Options));
complete({module, [Option]}) -> atom_to_list(Option);
complete({module, Options}) -> tm_menu:get_selection(Options);
complete(_) -> "".

display([{call, {Function, Arity}}|Options]) ->
    [lists:flatten(io_lib:format("~w/~w", [Function, Arity]))|display(Options)];
display([{module, Module}|Options]) -> [Module|display(Options)];
display([_|Options]) -> display(Options);
display([]) -> [].

calls(Options) ->
    Calls = [lists:flatten(io_lib:format("~w/~w", [F, A])) || {F, A} <- Options],
    option(tm_menu:get_selection(Calls), lists:zip(Options, Calls)).

option(Selection, [{Option, Selection}|_]) -> Option;
option(Selection, [_|OptionSelections]) -> option(Selection, OptionSelections).

snippet({call,{F, A}}) -> io_lib:format("~w(~s)", [F, args(A)]).

args(Arity) -> args(Arity, 0).

args(Arity, Arity) -> [];
args(Arity, N) ->
    [io_lib:format("${~w:Arg~w}, ", [N+1, N+1])|
    args(Arity, N + 1)].