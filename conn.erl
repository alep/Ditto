-module(conn).
-import(ping).
-import(lists, [concat/1, keysearch/3]).
-export([conn/2]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2]). 
-behaviour(gen_event).

conn(Host, PortNo) ->
    Result = gen_tcp:connect(Host, PortNo, [binary,
                                            {active, true},
                                            {packet, line},
                                            {keepalive, true},
                                            {nodelay, true}]),
    case Result of
        {ok, Socket} ->
            Pid = spawn(fun() -> loop(Socket) end),
            gen_tcp:controlling_process(Socket, Pid),
            Pid;
        {error, Reason} ->
            io:format("Error, reason: ~p~n", [Reason]),
            error
    end.

loop(Socket) ->
    receive
        {tcp, _Socket, Data} ->
            gen_event:notify(ditto, {recv, Data}), 
            loop(Socket);
        {tcp_closed, _Socket} ->
            io:format("[SOCKET] Connection Closed.~n"),
            ok;
        {tcp_error, _Socket, Reason} ->
            io:format("[SOCKET] Error, reason: ~p~n", [Reason]),
            ok;
        {send, _Pid, Data} ->
            io:format("[SOCKET] Sending data: ~s~n", [Data]),
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {close, _Pid} ->
            gen_tcp:close(Socket),
            io:format("[SOCKET] Closing connection...~n"),
            ok;
        Msg ->
            io:format("[SOCKET] Recv: ~p~n", [Msg])
    end.


irc_msg(Args) ->
    lists:concat(Args) ++ "\r\n".

%
% gen_event behaviour
%
init(Args) ->
    io:format("*** initiating gen_event ***~n"),
    State = {disconnected, Args},
    {ok, State}.

handle_event({recv, Line}, State) ->
    io:format("~s", [binary_to_list(Line)]),
    {ok, State};

handle_event({send, Line}, {_, Pid}=State) ->
    Pid ! {send, self(), Line ++ "\r\n"},
    {ok, State};

handle_event({pass, Password}, {disconnected, Pid}) ->
    Pid ! {send, self(), irc_msg(["PASS ", Password])},
    {ok, {nick, Pid}};

handle_event({nick, Nickname}, {nick, Pid}) ->
    Pid ! {send, self(), irc_msg(["NICK ", Nickname])},
    {ok, {user, Pid}};

handle_event({user, Nickname, Realname}, {user, Pid}) ->
    Pid ! {send, self(), irc_msg(["USER ", Nickname, " 0 * :", Realname])},
    {ok, {connected, Pid}};

handle_event({privmsg, To, Msg}, {connected, Pid}) ->
    Pid ! {send, self(), irc_msg(["PRIVMSG ", To, " :", Msg])},
    {ok, {connected, Pid}};

handle_event({join, Channel}, {connected, Pid}) ->
    Pid ! {send, self(), irc_msg(["JOIN ", Channel])},
    {ok, {connected, Pid}};

handle_event(quit, {connected, Pid}) ->
    Pid ! {send, self(), irc_msg(["QUIT"])},
    {ok, {disconnected, Pid}};

handle_event(Event, State) ->
    io:format("*** Error Unknow Event: ~p ***~n", [Event]),
    {ok, State}.

handle_call(_Request, State) ->
    io:format("~p~n", [State]),
    {ok, State, State}.

handle_info(_Info, State) ->
    io:format("~p, my pid: ~n", [State]),
    {ok, State}.

terminate({stop, Reason}, {_, Pid}) ->
    io:format("Stoping: ~p~n", [Reason]),
    Pid ! {close, self()},
    ok;
terminate(stop, {_, Pid}) ->
    io:format("Stoping.. ~n"),
    Pid ! {close, self()},
    ok.

