-module (complete).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export ([string/1, string/2]).

% The 'complete' module provides Erlang code completion based text preceding
% the current cursor position. Any amount of previous context can be used
%
% Stateful completions (dialogues with the user) can be achieved by having a
% receive block within a complete function that takes the user's selection or
% further typing (for type-ahead support)
%
% A stateful interaction will be particularly useful for spawn(M, F, A) vs
% spawn(Node, M, F, A) where the user's selection affects the scope of
% subsequent completions. Without state there is no simple way to disambiguate
% "spawn(Node, M" from "spawn(M, F" since both become "spawn(<atom>,<atom>" in
% tokens and the same atom can be both a node name and a module name. However,
% knowing that the user selected a node for the first argument allows
% intelligent completion from there on.
%
% In principle type-ahead should be very easy in Erlang since a process
% can naturally track the progressively filtered options. Type-ahead timeout
% is easily modeled with a receive block's 'after' statement
%
% A decent design for this would accomodate different editor conventions and
% thus allow another module to define the format of a completion based on the
% user's selection. I would implement such a callback module for TextMate.
% Making the completion server run would be an exercise for the reader unless
% they are using TextMate where I will write a command that interacts with it
% and launches it on demand.
%
% Right off the bat the big value item is M:F(args) completion
% Beyond that, having the doc float alongside the M:F(args) completions is
% probably the next most generally useful piece of functionality
string(String) -> string([], String).
string(LocalModule, String) ->
    {ok, Tokens, 1} = erl_scan:string(String),
    complete(LocalModule, lists:reverse(Tokens)).

complete(_, [{atom,1,Function},{':',1},{atom,1,Module},{'fun',1}|_]) ->
    % all functions in fun M:F/Arity expression
    % listing covered by M:F expression, but this needs a different completion result
    % should complete to fun M:F/Arity
    [{fun_ref, F} || F <- lists:filter(starts_with(atom_to_list(Function)), Module:module_info(exports))];
complete(_, [{':',1},{atom,1,Module},{'fun',1}|_]) ->
    % all functions in fun M:F/Arity expression
    % listing covered by M:F expression, but this needs a different completion result
    % should complete to fun M:F/Arity
    [{fun_ref, F} || F <- Module:module_info(exports)];
complete(_, [{atom,1,Module},{'fun',1}|_]) ->
    % select modules in fun M:F/Arity expression
    % TODO: should also select local functions
    [{module, M} || M <- lists:filter(starts_with(atom_to_list(Module)), erlang:loaded())];
complete([], [{'fun',1}|_]) ->
    % select modules in fun M:F/Arity expression
    % TODO: should also select local functions
    [{module, M} || M <- erlang:loaded()];
complete(LocalModule, [{'fun',1}|_]) ->
    % select functions in fun F/Arity expression OR
    % select modules in fun M:F/Arity expression
    [{fun_ref, F} || F <- LocalModule:module_info(exports)] ++ [{module, M} || M <- erlang:loaded()];

complete(_, [{':',1},{atom,1,Module}|_]) ->
    % all functions in M:F expression
    % should complete to M:F(${0:arg1},${1:arg2},...)
    [{call, F} || F <- Module:module_info(exports)];
complete(_, [{atom,1,Function},{':',1},{atom,1,Module}|_]) ->
    % select functions in M:F expression
    % TODO: use spec/source of module to get doc and arg names
    [{call, F} || F <- lists:filter(starts_with(atom_to_list(Function)), Module:module_info(exports))];

complete(_, [{'(',1},{atom,1,spawn}|_]) ->
    % all modules in spawn(M,F,A) expression
    % should complete to spawn(M,
    % TODO: support spawn(Node, M, F, A) as well
    [{module, M} || M <- erlang:loaded()];
complete(_, [{atom,1,Module},{'(',1},{atom,1,spawn}|_]) ->
    % select modules in spawn(M,F,A) expression
    [{module, M} || M <- lists:filter(starts_with(atom_to_list(Module)), erlang:loaded())];
complete(_, [{',',1},{atom,1,Module},{'(',1},{atom,1,spawn}|_]) ->
    % all functions in spawn(M,F,A) expression
    % should complete to spawn(M, F,[${0:arg1},${1:arg2},...])
    [{function, F} || F <- Module:module_info(exports)];
complete(_, [{atom,1,Function},{',',1},{atom,1,Module},{'(',1},{atom,1,spawn}|_]) -> %
    % select functions in spawn(M,F,A) expression
    [{function, F} || F <- lists:filter(starts_with(atom_to_list(Function)), Module:module_info(exports))];

complete(_, [{atom,1,is},{'when',1}|_]) ->
    % all standard guard expressions
    % should complete to Guard(${0:arg1},${1:arg2},...)
    [{call, F} || F <- lists:filter(starts_with("is"), erlang:module_info(exports))];

complete(_, [{atom,1,Module}|_]) ->
    % select modules
    [{module, M} || M <- lists:filter(starts_with(atom_to_list(Module)), erlang:loaded())];

% FUTURE: automatic case clause generation based on specs/docs

complete(_, _) -> [].

starts_with(String) ->
    Len = length(String),
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
