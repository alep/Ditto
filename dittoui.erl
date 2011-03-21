-module(dittoui).

-import(conn).
-import(lists, [concat/1, keysearch/3]).

-export([start/0, stop/0, main/0]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2]).

-include_lib("wx/include/wx.hrl").

-behaviour(gen_event).

-define(CONNECT, ?wxID_OPEN).
-define(EXIT, ?wxID_EXIT).

-define(FRAME, 130).
-define(OK, 131).
-define(QUIT, 132).

-define(SERVEROUT, 133).
-define(COMMANDS, 134).
-define(SENDBUTTON, 135).

start() ->
    spawn(?MODULE, main, []).

stop() ->
    gen_event:stop(dittoui).

main() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?FRAME, "Ditto: An IRC client (ex-bot?)"),
    {Text, Log} = setup(Frame),
    wxFrame:show(Frame),
    loop(Frame, Text, Log, false),
    wx:destroy().

setup(Frame) ->
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),

    wxMenu:append(File, ?CONNECT, "Connect"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?EXIT, "Quit"),
   
    wxMenuBar:append(MenuBar, File, "&File"),
    wxFrame:setMenuBar(Frame, MenuBar),
   
    wxFrame:createStatusBar(Frame),

    wxFrame:setStatusText(Frame, "Welcome To wxErlang"),
   
    %% Setup sizers
    Panel = wxPanel:new(Frame),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    OutputSizer = wxBoxSizer:new(?wxHORIZONTAL),
    MsgSizer = wxBoxSizer:new(?wxHORIZONTAL),


    ServerOut = wxTextCtrl:new(Panel, ?SERVEROUT, 
                               [{style, ?wxTE_MULTILINE}]),
    OkBtn = wxButton:new(Panel, ?SENDBUTTON, [{label, "Send"}]),
    Cmds = wxTextCtrl:new(Panel, ?COMMANDS, [{style, ?wxTE_PROCESS_ENTER}]),
    
    wxTextCtrl:setEditable(ServerOut, false),

    wxSizer:add(OutputSizer, ServerOut, [{proportion, 1}, 
                                         {flag, ?wxEXPAND bor ?wxALL}, 
                                         {border, 5}]),

    wxSizer:add(MsgSizer, Cmds, [{proportion, 1}, 
                                 {flag, ?wxALL}, 
                                 {border, 5}]),

    wxSizer:add(MsgSizer, OkBtn, [{proportion, 0}, 
                                  {flag, ?wxALL}, 
                                  {border, 5}]),    
    
    wxSizer:add(MainSizer, OutputSizer, [{proportion, 1}, 
                                         {flag, ?wxEXPAND}, 
                                         {border, 5}]),

    wxSizer:add(MainSizer, MsgSizer, [{proportion, 0}, 
                                      {flag, ?wxEXPAND bor ?wxTOP},
                                      {border, 5}]),

    wxSizer:setSizeHints(MainSizer, Frame),
    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:fit(MainSizer, Panel),

    wxFrame:connect(Frame, command_button_clicked),
    wxFrame:connect(Frame, command_text_enter),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window),
    
    {Cmds, ServerOut}.

% loop helpers

parse_and_notify_cmds(String) ->
    S = string:strip(String, both, $\n),
    Op = getops(S),
    case Op of
        {join, Channel} ->
            gen_event:notify(ditto, Op),
            "*** join" ++ Channel;       
        {privmsg, To, Msg} ->
            gen_event:notify(ditto, Op),
            "*** say: " ++ Msg ++ " to: " ++ To ;
        {quit} ->
            gen_event:notify(ditto, quit),
            "*** quit";
        _ ->
            "*** error: unknown command."
    end.

getops(String) ->
    AvailOps = [{"/join", join, 1}, {"/quit", quit, 0}, {"/msg", privmsg, 2}],
    Msg = string:tokens(String, "\t\n "),
    Key = lists:nth(1, Msg),
    case keysearch(Key, 1, AvailOps) of
        {value, {"/msg", privmsg, 2}} ->
            {privmsg, lists:nth(2, Msg),
             string:join(lists:nthtail(2, Msg), " ")};
        {value, {Key, Op, Len}} ->
            list_to_tuple([Op] ++ lists:sublist(Msg, 2, Len));
        _ ->
            Msg
    end.

aux_send_cmd(TextCtrlLog, TextCtrlCmd, String) ->
    Op = parse_and_notify_cmds(String),
    wxTextCtrl:writeText(TextCtrlLog, Op ++ "\n"),
    wxTextCtrl:clear(TextCtrlCmd),
    io:format("[DEBUG] Command: ~p~n", [String]),
    ok.

aux_gen_event_start(SocketPid) ->
    gen_event:start({local, ditto}),
    gen_event:add_handler(ditto, conn, SocketPid),
    gen_event:add_handler(ditto, ping, SocketPid),
    gen_event:add_handler(ditto, ?MODULE, [SocketPid, self()]),   % self() is the "UI Pid"
    ok.

aux_parse_modaldialog_string(Dialog, TextCtrlLog) ->
    case wxTextEntryDialog:showModal(Dialog) of
        ?wxID_OK ->
            Str = wxTextEntryDialog:getValue(Dialog),
            wxTextCtrl:writeText(TextCtrlLog, ("Connecting to " ++ Str ++ "\n")),
            io:format("[DEBUG] Server: ~p~n", [Str]),
            %% [Host|[Port|_Xs]] = string:tokens(Str, ":"),
            %% PortNo = list_to_integer(Port),
            Host = "chat.freenode.net",
            PortNo = 6667,
            {Host, PortNo};
        _ -> 
            error
    end.


loop(Frame, Text, Log, Connected) ->
    receive
        #wx{id=?COMMANDS, event=#wxCommand{type=command_text_enter,
                                           cmdString=String}} ->
            aux_send_cmd(Log, Text, String),
            loop(Frame, Text, Log, Connected);

        #wx{id=?SENDBUTTON, event=#wxCommand{type=command_button_clicked}} ->
            String = wxTextCtrl:getValue(Text),
            aux_send_cmd(Log, Text, String),
            loop(Frame, Text, Log, Connected);

        #wx{id=?CONNECT, event=#wxCommand{}} ->
            case Connected of 
                true ->
                    NextConnected = true;
                false ->
                    Prompt = "Connect to server:",
                    MD = wxTextEntryDialog:new(Frame, Prompt, [{caption, "Connect to..."}]),                   
                    ConnData = aux_parse_modaldialog_string(MD, Log),
                    case ConnData of
                        {Host, PortNo} ->
                            SockPid = conn:conn(Host, PortNo),
                            case SockPid of
                                error ->
                                    NextConnected = false,
                                    error;
                                _ ->
                                    aux_gen_event_start(SockPid),
                                    timer:sleep(1000),
                                    io:format("*** Registering ***~n"),
                                    gen_event:notify(ditto, {pass, "chumbawamba"}),
                                    gen_event:notify(ditto, {nick, "aleperaltabot"}),
                                    gen_event:notify(ditto, {user, "aleperaltabot", 
                                                             "Ale Peralta's Client"}),
                                    NextConnected = true
                            end;
                        error ->
                            NextConnected = false,
                            error
                    end,                 
                    wxDialog:destroy(MD)
            end,
            loop(Frame, Text, Log, NextConnected);


        #wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
            gen_event:stop(dittoui),
            io:format("Bye.");

        #wx{id=?FRAME, event=#wxClose{type=close_window}} ->
            gen_event:stop(dittoui),
            io:format("Bye.");
        
        {server, Line} ->
            wxTextCtrl:writeText(Log, Line ++ "\n"),
            loop(Frame, Text, Log, Connected);
        
        Msg ->
            io:format("[DEBUG] Lost message: ~p~n", [Msg]),
            loop(Frame, Text, Log, Connected)
    end.


% helpers to callbacks to the gen_event behaviour

get_elem_or_error(Key, List) ->
    case keysearch(Key, 1, List) of
        {value, {Key, Pid}} ->
            Pid;
        _ ->
            exit("Error missing elem")
    end.


get_conn(State) ->
    Key = conn,
    get_elem_or_error(Key, State).

    
get_ui(State) ->
    Key = ui,
    get_elem_or_error(Key, State).


% callbacks to the gen_event behaviour


init(Args) ->
    [ConnPid|[DittoUIPid|_Xs]] = Args,
    {ok, [{conn, ConnPid}, {ui, DittoUIPid}]}.


handle_event({recv, Data}, State) -> 
    S0 = binary_to_list(Data),
    S1 = string:strip(S0, both, $\n),
    Line = string:strip(S1, both, $\r),
    Pid = get_ui(State),
    Pid ! {server, Line},
    {ok, State};
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

