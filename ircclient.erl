-module(ircclient).
-import(lists, [concat/1, keysearch/3]).
-import(ircmsg, [irc_msg/1, irc_parsemsg/1, get_command_from_num/1]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2, code_change/3]). 
-export([handle_RPL_MOTD/3, handle_RPL_ENDOFMOTD/3, handle_RPL_MOTDSTART/3,
        handle_PING/3]).
-behaviour(gen_event).

% helpers to callbacks to the gen_event behaviour

get_elem_or_error(Key, List) ->
    case keysearch(Key, 1, List) of
        {value, {Key, Value}} ->
            Value;
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

clean_line(Data) ->
    S0 = binary_to_list(Data),
    S1 = string:strip(S0, both, $\n),
    string:strip(S1, both, $\r).

init(Args) ->
    %% Args = [{conn, ConnPid}, {ui, DittoUIPid}]
    {ok, {disconnected, Args}}.

handle_event({recv, Data}, {_, Vars}=State) -> 
    Line = clean_line(Data),
    case irc_parsemsg(Line) of
        {ok, ParsedLine} ->
            UIPid = get_ui(Vars),
            if not (UIPid =:= error) ->
                    UIPid ! {server, ParsedLine}
            end,
            R = handle_msg(ParsedLine, State);
        {error, _} ->
            R = State
    end,  
    {ok, R};

%% handle_event({recv, Data}, {motdstart, Vars}=State) ->
%%     Line = clean_line(Data),
%%     case irc_parsemsg(Line) of
%%         {ok, ParsedLine} ->
%%             handle_msg(ParsedLine, State);
%%         {error, _} ->
%%             ok
%%     end,
%%     {ok, State};

%% handle_event({recv, Data}, {motd, Vars}=State) ->
    
%%     {ok, State};

%% handle_event({recv, Data}, {endofmotd, Vars}=State} ->
    
%%     {ok, {connected, Vars}};


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

handle_msg({Prefix, Command, Args}, State) ->
    CommandName = get_command_from_num(Command),

    Name = list_to_atom("handle_" ++ CommandName),
    Result = (catch apply(?MODULE, Name, [Prefix, Args, State])),

    case Result of
        {'EXIT', {undef, _}} ->
            handle_unknown_command(Prefix, CommandName, Args),
            State;
        _ ->
            io:format("Result: ~p~n", [Result]),
            Result
    end.

handle_PING(_Prefix, Args, {St, Vars}) ->
    send_msg(Vars, irc_msg(["PONG"] ++ Args)),
    {St, Vars}.

handle_RPL_MOTDSTART(_Prefix, _Args, {St, Vars}) ->
    NewVars = [{motd, []} | Vars],
    {St, NewVars}.

handle_RPL_MOTD(_Prefix, Args, {St, Vars}=State) ->
    MOTD = get_elem_or_error(motd, Vars),
    case (MOTD =:= error) of
        true ->
            State;
        false ->
            NewMOTD = [Args|MOTD],
            V = lists:keydelete(motd, 1, Vars),
            NewVars = [{motd, NewMOTD}|V],
            {St, NewVars}
    end.

handle_RPL_ENDOFMOTD(_Prefix, _Args, {St, Vars}=State) ->
    MOTD = get_elem_or_error(motd, Vars),
    case (MOTD =:= error) of
        true ->
            State;
        false ->
            NewMOTD = lists:reverse(MOTD),
            V = lists:keydelete(motd, 1, Vars),
            NewVars = [{motd, NewMOTD}|V],
            {St, NewVars}
    end.

handle_unknown_command(_Prefix, Command, _Args) ->
    io:format("Unknown command: ~p~n", [Command]),
    error.
