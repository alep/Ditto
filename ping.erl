-module(ping).
-import(string, [strip/1, strip/3]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2]).
-export([is_ping_cmd/1]).
-behaviour(gen_event).


init(Args) ->
    io:format("*** initating gen_event, to handle pings ***"),
    {ok, Args}.


is_ping_cmd(_Line="PING"++RestLine) ->
    {true, strip(strip(RestLine), both, $:)};
is_ping_cmd(_) ->
    {false, ""}.

handle_event({recv, Data}, State) -> 
    Line = binary_to_list(Data),
    case is_ping_cmd(Line) of
        {true, Rest} ->
            io:format("PING HANDLER: ~s", [Line]),
            Arg = string:strip(Rest),
            State ! {send, self(), "PONG " ++ Arg ++ "\r\n"},
            {ok, State};
        _ -> 
            {ok, State}
    end;
handle_event(_Ignore, State) ->
    {ok, State}.


handle_call(_Request, State) ->
    io:format("~p~n", [State]),
    {ok, State, State}.

handle_info(_Info, State) ->
    io:format("~p, my pid: ~n", [State]),
    {ok, State}.


terminate({stop, Reason}, _State) ->
    io:format("Stoping: ~p~n", [Reason]),
    ok;
terminate(stop, _State) ->
    io:format("Stoping.. ~n"),
    ok.

