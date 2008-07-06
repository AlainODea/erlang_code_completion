-module (tm_complete).
-export ([string/1, string/2]).

string(String) -> snippet(users_choice(complete:string(String))).
string(Local, String) -> snippet(users_choice(complete:string(Local, String))).

users_choice([Option]) -> Option;
users_choice(Options) ->
    Menu = menu(Options),
    option(tm_menu:selection(Menu), Options, Menu).

menu([{fun_ref, {Function, Arity}}|Options]) ->
    [lists:flatten(io_lib:format("~w/~w", [Function, Arity]))|menu(Options)];
menu([{call, {Function, Arity}}|Options]) ->
    [lists:flatten(io_lib:format("~w/~w", [Function, Arity]))|menu(Options)];
menu([{module, Module}|Options]) -> [atom_to_list(Module)|menu(Options)];
menu([_|Options]) -> menu(Options);
menu([]) -> [].

option([], _, _) -> "";
option(Selection, [Option|_], [Selection|_]) -> Option;
option(Selection, [_|Options], [_|Menu]) -> option(Selection, Options, Menu).

snippet({fun_ref, {F, A}}) -> io_lib:format("~w/~w", [F, A]);
snippet({call, {F, A}}) -> io_lib:format("~w(~s)", [F, args(A)]);
snippet({module, M}) -> atom_to_list(M);
snippet(_) -> "".

args(Arity) -> args(Arity, 1).
args(Arity, Arity) -> io_lib:format("${~w:Arg~w}", [Arity, Arity]);
args(Arity, N) ->
    [io_lib:format("${~w:Arg~w}, ", [N, N])|
    args(Arity, N + 1)].
