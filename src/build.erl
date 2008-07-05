-module(build).
-export([exec/2]).

exec(tm_command, Iter) ->
    File = Iter(),
    annotate(compile:file(File, [return]), Iter).

annotate({ok, _, []}, Iter) ->
    passthru(Iter(), Iter);
annotate({ok, _, Warnings}, Iter) ->
    annotate(lists:keysort(1,
        lists:flatten([
            lists:foldl(
                fun({_, SubErrs}, AccIn) -> [SubErrs|AccIn] end, [], Warnings
            )
        ])
    ), 0, Iter(), Iter);
annotate({error, Errors, Warnings}, Iter) ->
    annotate(lists:keysort(1,
        lists:flatten([
            lists:foldl(
                fun({_, SubErrs}, AccIn) -> [SubErrs|AccIn] end, [], Warnings
            )
            |lists:flatten(
                lists:foldl(
                    fun({_, SubErrs}, AccIn) -> [SubErrs|AccIn] end, [], Errors
                )
            )
        ])
    ), 0, Iter(), Iter).

passthru(eof, _) ->
    [];
passthru(Line, Iter) ->
    [Line|passthru(Iter(), Iter)].

% no more lines in the source file
annotate(_, _, eof, _) ->
    [];
% no more annotations
annotate([], _, Line, Iter) ->
    [Line|passthru(Iter(), Iter)];
% annotation for current line
annotate([{LineNumber, Message}|Annotations], LineNumber, Line, Iter) ->
    [format(Line, Message)|annotate(Annotations, LineNumber, Iter(), Iter)];
% no annotation for current line
annotate(Annotations, LineNumber, Line, Iter) ->
    [Line|annotate(Annotations, LineNumber+1, Iter(), Iter)].

format(Line, Message) ->
    io_lib:format("~s% ~s~n", [Line, Message]).
