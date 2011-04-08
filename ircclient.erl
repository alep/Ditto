-module(ircclient).
-import(lists, [concat/1, keysearch/3]).
-import(ircmsg, [irc_msg/1, irc_parsemsg/1]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2, code_change/3]). 
-behaviour(gen_event).

% helpers to callbacks to the gen_event behaviour

get_elem_or_error(Key, List) ->
    case keysearch(Key, 1, List) of
        {value, {Key, Pid}} ->
            Pid;
        _ ->
            io:format("Error, key: ~p not fount~n", [Key]),
            error
    end.

get_conn(State) ->
    Key = conn,
    get_elem_or_error(Key, State).
   
get_ui(State) ->
    Key = ui,
    get_elem_or_error(Key, State).

% send message to process handling the socket

send_msg(Vars, Msg) ->
    ConnPid = get_conn(Vars),
    if not (ConnPid =:= error) ->
            ConnPid ! {send, self(), Msg}
    end.

close_conn(Vars) ->
    ConnPid = get_conn(Vars),
    if not (ConnPid =:= error) ->
            ConnPid ! {close, self()}
    end.

% callbacks to the gen_event behaviour

init(Args) ->
    %% Args = [{conn, ConnPid}, {ui, DittoUIPid}]
    {ok, {disconnected, Args}}.

handle_event({recv, Data}, {_, Vars}=State) -> 
    S0 = binary_to_list(Data),
    S1 = string:strip(S0, both, $\n),
    Line = string:strip(S1, both, $\r),

    UIPid = get_ui(Vars),
    if not (UIPid =:= error) ->
            UIPid ! {server, Line}
    end,
    {ok, State};
handle_event({send, Line}, {_, Vars}=State) ->
    send_msg(Vars, Line ++ "\r\n"),
    {ok, State};
handle_event({pass, Password}, {disconnected, Vars}) ->
    send_msg(Vars, irc_msg(["PASS", Password])),   
    {ok, {nick, Vars}};

handle_event({nick, Nickname}, {nick, Vars}) ->
    send_msg(Vars, irc_msg(["NICK", Nickname])),
    {ok, {user, Vars}};

handle_event({user, Nickname, Host, Server, Realname}, {user, Vars}) ->
    send_msg(Vars, irc_msg(["USER", Nickname, Host,
                                Server, "0 * :", Realname])),
    {ok, {connected, Vars}};

handle_event({privmsg, To, Msg}, {connected, Vars}) ->
    send_msg(Vars, irc_msg(["PRIVMSG", To, ":", Msg])),
    {ok, {connected, Vars}};

handle_event({join, Channel}, {connected, Vars}) ->
    send_msg(Vars, irc_msg(["JOIN", Channel])),
    {ok, {connected, Vars}};

handle_event(quit, {connected, Vars}) ->
    send_msg(Vars, irc_msg(["QUIT"])),
    {ok, {disconnected, Vars}};

handle_event(Event, State) ->
    io:format("*** Error Unknow Event: ~p ***~n", [Event]),
    {ok, State}.

handle_call(_Request, State) ->
    io:format("~p~n", [State]),
    {ok, State, State}.

handle_info(_Info, State) ->
    io:format("~p, my pid: ~n", [State]),
    {ok, State}.

terminate({stop, Reason}, {_, Vars}) ->
    io:format("Stoping: ~p~n", [Reason]),
    close_conn(Vars),
    ok;
terminate(stop, {_, Vars}) ->
    io:format("Stoping.. ~n"),
    close_conn(Vars),
    ok.

code_change(_, _, _) ->
    ok.
