-module(ircmsg).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

get_args(Str) ->
    Pos = string:str(Str, " :"),
    case Pos > 0 of
        false ->
            Args = string:tokens(Str, " ");
        true ->
            Trailling = string:substr(Str, Pos+2), 
            S = lists:sublist(Str, Pos),
            A = string:tokens(S, " "),
            Args = A ++ [Trailling]
    end,
    Args.

irc_parsemsg("") ->
    {error, "Empty line."};
irc_parsemsg(":"++S) ->
    Idx = string:chr(S, $ ),
    Prefix = lists:sublist(S, Idx - 1),
    S0 = string:substr(S, Idx + 1),
    Args0 = get_args(S0),
    [Command|Args] = Args0,
    {ok, {Prefix, Command, Args}};
irc_parsemsg(S) ->
    Prefix = "",
    Args0 = get_args(S),
    [Command|Args] = Args0,
    {ok, {Prefix, Command, Args}}.
    
%%
%% Tests
%%
privmsg_1_test() ->
    S = "PRIVMSG Angel :yes I\'m receiving it !",
    Result = irc_parsemsg(S),
    TestData = {ok, {[], "PRIVMSG", ["Angel", "yes I'm receiving it !"]}},
    ?assertEqual(Result, TestData).

privmsg_2_test() ->
    S = "PRIVMSG kalt%millennium.stealth.net :Do you like cheese?",
    Result = irc_parsemsg(S),
    TestData = {ok, {[], "PRIVMSG", ["kalt%millennium.stealth.net", 
                                     "Do you like cheese?"]}},
    ?assertEqual(Result, TestData).  

privmsg_3_test() ->
    S = "PRIVMSG kalt%millennium.stealth.net ::Do you like cheese?",
    Result = irc_parsemsg(S),
    TestData = {ok, {[], "PRIVMSG", ["kalt%millennium.stealth.net", 
                                     ":Do you like cheese?"]}},
    ?assertEqual(Result, TestData).

kick_1_test() ->
    S = ":WiZ!jto@tolsun.oulu.fi KICK #Finnish John",
    Result = irc_parsemsg(S),
    TestData = {ok, {"WiZ!jto@tolsun.oulu.fi", "KICK", ["#Finnish", "John"]}},
    ?assertEqual(Result, TestData).
