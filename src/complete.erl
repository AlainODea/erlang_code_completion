-module (complete).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export ([string/1, string/2]).

string(String) -> string([], String).
string(Local, String) ->
    {ok, Tokens, 1} = erl_scan:string(String),
    complete(Local, lists:reverse(Tokens)).

complete(_, [{atom,1,Function},{':',1},{atom,1,Module},{'fun',1}|_]) ->
    % select functions in fun M:F/Arity expression
    Prefix = atom_to_list(Function),
    {string:len(Prefix),[{fun_ref, F} || F <- lists:filter(starts_with(Prefix), Module:module_info(exports))]};
complete(_, [{':',1},{atom,1,Module},{'fun',1}|_]) ->
    % all functions in fun M:F/Arity expression
    {0,[{fun_ref, F} || F <- Module:module_info(exports)]};
complete([], [{atom,1,FunctionOrModule},{'fun',1}|_]) ->
    % select modules in fun M:F/Arity expression
    Prefix = atom_to_list(FunctionOrModule),
    {string:len(Prefix), [{fun_ref, F} || F <- lists:filter(starts_with(Prefix), builtins())] ++
    [{module, M} || M <- lists:filter(starts_with(Prefix), lists:sort(erlang:loaded()))]};
complete(Local, [{atom,1,FunctionOrModule},{'fun',1}|_]) ->
    % select local functions in fun F/Arity expression
    % select modules in fun M:F/Arity expression
    Prefix = atom_to_list(FunctionOrModule),
    {string:len(Prefix), [{fun_ref, F} || F <- lists:filter(starts_with(Prefix), Local:module_info(exports) ++ builtins())] ++
    [{module, M} || M <- lists:filter(starts_with(Prefix), lists:sort(erlang:loaded()))]};
complete([], [{'fun',1}|_]) ->
    % select modules in fun M:F/Arity expression
    {0, [{fun_ref, F} || F <- builtins()] ++
    [{module, M} || M <- lists:sort(erlang:loaded())]};
complete(Local, [{'fun',1}|_]) ->
    % select functions in fun F/Arity expression OR
    % select modules in fun M:F/Arity expression
    {0, [{fun_ref, F} || F <- Local:module_info(exports)] ++
    [{module, M} || M <- lists:sort(erlang:loaded())]};

complete(_, [{':',1},{atom,1,Module}|_]) ->
    % all functions in M:F expression
    {0, [{call, F} || F <- Module:module_info(exports)]};
complete(_, [{atom,1,Function},{':',1},{atom,1,Module}|_]) ->
    % select functions in M:F expression
    Prefix = atom_to_list(Function),
    {string:len(Prefix), [{call, F} || F <- lists:filter(starts_with(Prefix), Module:module_info(exports))]};

complete(_, [{'(',1},{atom,1,spawn}|_]) ->
    % all modules in spawn(M,F,A) expression
    % should complete to spawn(M,
    % TODO: support spawn(Node, M, F, A) as well
    {0, [{module, M} || M <- lists:sort(erlang:loaded())]};
complete(_, [{atom,1,Module},{'(',1},{atom,1,spawn}|_]) ->
    % select modules in spawn(M,F,A) expression
    Prefix = atom_to_list(Module),
    {string:len(Prefix), [{module, M} || M <- lists:filter(starts_with(Prefix), lists:sort(erlang:loaded()))]};
complete(_, [{',',1},{atom,1,Module},{'(',1},{atom,1,spawn}|_]) ->
    % all functions in spawn(M,F,A) expression
    % should complete to spawn(M, F,[${0:arg1},${1:arg2},...])
    {0, [{function, F} || F <- Module:module_info(exports)]};
complete(_, [{atom,1,Function},{',',1},{atom,1,Module},{'(',1},{atom,1,spawn}|_]) -> %
    % select functions in spawn(M,F,A) expression
    Prefix = atom_to_list(Function),
    {string:len(Prefix), [{function, F} || F <- lists:filter(starts_with(Prefix), Module:module_info(exports))]};

complete(_, [{atom,1,is},{'when',1}|_]) ->
    % all standard guard expressions
    % TODO: support all allowable guard functions like length
    % TODO: support compound guard expressions: when is_list(L); length(List) == 6; is_tuple(T)
    {0, [{call, F} || F <- lists:filter(starts_with("is_"), builtins())]};

complete([], [{atom,1,FunctionOrModule}|_]) ->
    % select modules
    Prefix = atom_to_list(FunctionOrModule),
    {string:len(Prefix), [{call, F} || F <- lists:filter(starts_with(Prefix), builtins())] ++
    [{module, M} || M <- lists:filter(starts_with(Prefix), lists:sort(erlang:loaded()))]};
complete(Local, [{atom,1,FunctionOrModule}|_]) ->
    % select builtin calls
    % select local calls
    % select modules
    Prefix = atom_to_list(FunctionOrModule),
    {string:len(Prefix), [{call, F} || F <- lists:filter(starts_with(Prefix), Local:module_info(exports) ++ builtins())] ++
    [{module, M} || M <- lists:filter(starts_with(Prefix), lists:sort(erlang:loaded()))]};

complete(_, _) -> {0, []}.

builtins() ->
    lists:filter(fun({F,A}) -> erlang:is_builtin(erlang, F, A) end, erlang:module_info(exports)).

starts_with(String) ->
    Len = string:len(String),
    fun({Atom,_}) ->
        case string:substr(atom_to_list(Atom), 1, Len) of
            String -> true;
            _ -> false
        end;
    (Atom) ->
        case string:substr(atom_to_list(Atom), 1, Len) of
            String -> true;
            _ -> false
        end
    end.
