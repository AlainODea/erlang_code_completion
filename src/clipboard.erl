-module(clipboard).
-export([copy/1]).

copy(Data) ->
    Clipboard = open_port({spawn, "pbcopy"}, [stream]),
    Clipboard ! {self(), {command, Data}},
    Clipboard ! {self(), close},
    receive
        {Clipboard, closed} -> true
    end.
