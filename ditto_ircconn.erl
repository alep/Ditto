-module(conn).
-import(lists, [concat/1, keysearch/3]).
-export([open/3]).


open(Host, PortNo, Fun) ->
    Result = gen_tcp:connect(Host, PortNo, [binary,
                                            {active, true},
                                            {packet, line},
                                            {keepalive, true},
                                            {nodelay, true}]),
    case Result of
        {ok, Socket} ->
            Pid = spawn(fun() -> loop(Socket, Fun) end),
            gen_tcp:controlling_process(Socket, Pid),
            Pid;
        {error, Reason} ->
            io:format("Error, reason: ~p~n", [Reason]),
            error
    end.


loop(Socket, Fun) ->
    receive
        {tcp, _Socket, Data} ->
            Fun({recv, Data}),
            loop(Socket, Fun);
        {tcp_closed, _Socket} ->
            io:format("[SOCKET] Connection Closed.~n"),
            ok;
        {tcp_error, _Socket, Reason} ->
            io:format("[SOCKET] Error, reason: ~p~n", [Reason]),
            ok;
        {send, _Pid, Data} ->
            io:format("[SOCKET] Sending data: ~s~n", [Data]),
            gen_tcp:send(Socket, Data),
            loop(Socket, Fun);
        {close, _Pid} ->
            gen_tcp:close(Socket),
            io:format("[SOCKET] Closing connection...~n"),
            ok;
        Msg ->
            io:format("[SOCKET] Recv: ~p~n", [Msg]),
            loop(Socket, Fun)
    end.
