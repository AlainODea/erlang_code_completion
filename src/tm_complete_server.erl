-module (tm_complete_server).
-export([start/0]).

start() -> spawn(fun run/0).

run() ->
    {ok, Listen} = gen_tcp:listen(2345, [list, {packet, line},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> par_connect(Listen) end),
    receive die -> ok end.

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    complete(Socket).

complete(Socket) ->
    receive
        {tcp, Socket, Header} ->
            {Module, Paths} = term(Header),
            [code:add_path(Path) || Path <- Paths],
            Completion = tm_complete:string(Module, string(Socket)),
            gen_tcp:send(Socket, Completion);
        _ -> ok
    end.

term(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    erl_parse:parse_term(Tokens).

string(Socket) -> lists:flatten(gather(Socket)).
gather(Socket) ->
    receive
        {tcp, Socket, [$\f|_]} -> [];
        {tcp, Socket, [$-|_]} -> gather(Socket);
        {tcp, Socket, Data} ->
            {ok, Cleaned, _} = regexp:gsub(Data, "[\r\n]", ""),
            [Cleaned|gather(Socket)];
        _ -> []
    end.
