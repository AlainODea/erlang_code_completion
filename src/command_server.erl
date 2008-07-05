-module(command_server).
-export([start/0]).

start() ->
    {ok, Listen} = gen_tcp:listen(2345, [list, {packet, line},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("Client session started~n"),
    spawn(fun() -> par_connect(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, ModuleRaw} ->
            io:format("Module:~s~n", [ModuleRaw]),
            Module = list_to_atom(trim(ModuleRaw)),
            Reply = Module:exec(tm_command, iter(Socket)),
            clipboard:copy(Reply),
            io:format("~w~n", [Reply]),
            gen_tcp:send(Socket, list_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Client session ended~n")
    end.

iter(Socket) ->
    fun() ->
        receive
            {tcp, Socket, "eof\n"} ->
                eof;
            {tcp, Socket, Line} ->
                trim(Line);
            {tcp_closed, Socket} ->
                eof
        end
    end.

trim(String) ->
    {ok, Trimmed, _} = regexp:gsub(String, "[\r\n]", ""),
    string:strip(Trimmed).
