-module(dittoui).

-import(conn).
-import(ircclient).
-import(ping).
-import(lists, [concat/1, keysearch/3]).

-export([start/0, main/0]).

-include_lib("wx/include/wx.hrl").

-define(EXIT, ?wxID_EXIT).

-define(FRAME, 130).
-define(OK, 131).
-define(QUIT, 132).

-define(SERVEROUT, 133).
-define(COMMANDS, 134).
-define(SENDBUTTON, 135).

% custom dialog
-define(CONNECT, 136).
-define(MDLOKBTN, 137).


start() ->
    spawn(?MODULE, main, []).

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


dialog_callback(EvtRecord, EvtObject) ->
    io:format("[event object]~p~n", [EvtObject]),
    case EvtRecord of 
        #wx{id=?MDLOKBTN, obj=Obj, 
            event=#wxCommand{type=command_button_clicked}} ->
            wxDialog:endModal(Obj, ?wxID_OK);
        #wx{obj=Obj} ->
            io:format("Other"),
            wxDialog:endModal(Obj, ?wxID_EXIT)
    end.


connect_dialog(Parent, Id, Title) ->
    Dialog = wxDialog:new(Parent, Id, Title, [{size, {275, 125}}]),

    Grid = wxGridSizer:new(5, 2, 0, 0), 
    
    HostLabel = wxStaticText:new(Dialog, ?wxID_ANY, "Host"),
    HostTxtInput = wxTextCtrl:new(Dialog, ?wxID_ANY),  
    
    PortLabel = wxStaticText:new(Dialog, ?wxID_ANY, "Port"),
    PortTxtInput = wxTextCtrl:new(Dialog, ?wxID_ANY),

    UserLabel = wxStaticText:new(Dialog, ?wxID_ANY, "Username"),
    UserTxtInput = wxTextCtrl:new(Dialog, ?wxID_ANY),
    
    NickLabel = wxStaticText:new(Dialog, ?wxID_ANY, "Nick"),
    NickTxtInput = wxTextCtrl:new(Dialog, ?wxID_ANY),  

    OkBtn = wxButton:new(Dialog, ?MDLOKBTN, [{label, "Connect"}]),
    CloseBtn = wxButton:new(Dialog, ?wxID_ANY, [{label, "Close"}]),
    
    BtnOpts = [{proportion, 0}, {flag, (?wxALIGN_CENTER bor 
                                        ?wxTOP bor ?wxBOTTOM)}],
    LblOPts = [{proportion, 0}, {flag, ?wxALIGN_CENTER}],
    TxtOpts = [{proportion, 0}, {flag, ?wxEXPAND}],  

    wxSizer:add(Grid, HostLabel, LblOPts),
    wxSizer:add(Grid, HostTxtInput, TxtOpts),
    wxSizer:add(Grid, PortLabel, LblOPts),
    wxSizer:add(Grid, PortTxtInput, TxtOpts),
    wxSizer:add(Grid, UserLabel, LblOPts),
    wxSizer:add(Grid, UserTxtInput, TxtOpts),
    wxSizer:add(Grid, NickLabel, LblOPts),
    wxSizer:add(Grid, NickTxtInput, TxtOpts),
    wxSizer:add(Grid, OkBtn,  BtnOpts),
    wxSizer:add(Grid, CloseBtn,  BtnOpts),
    
    wxDialog:setSizer(Dialog, Grid),
    wxDialog:connect(Dialog, command_button_clicked,
                     [{callback, fun dialog_callback/2}]),

    {Dialog, HostTxtInput, PortTxtInput, UserTxtInput, NickTxtInput}.

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
    gen_event:add_handler(ditto, ircclient, [{conn, SocketPid}, {ui, self()}]),
    ok.

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
                    {Dialog, HostTxt, PortTxt, UserTxt, NickTxt} = 
                        connect_dialog(Frame, ?wxID_ANY, "Connect"),
                    case wxDialog:showModal(Dialog) of 
                        ?wxID_OK  ->
                            HostVal = wxTextCtrl:getValue(HostTxt),
                            PortVal = wxTextCtrl:getValue(PortTxt),
                            UserVal = wxTextCtrl:getValue(UserTxt),
                            NickVal = wxTextCtrl:getValue(NickTxt),
                            io:format("Connection to ~p:~p with nick: ~p~n", 
                                      [HostVal, PortVal, NickVal]),
                            SockPid = conn:open(HostVal, 
                                                list_to_integer(PortVal),
                                               fun (X) ->
                                                       gen_event:notify(ditto, X)
                                               end),
                            case SockPid of
                                error ->
                                    NextConnected = false,
                                    error;
                                _ ->
                                    aux_gen_event_start(SockPid),
                                    io:format("*** Registering ***~n"),
                                    {ok, Hostname} = inet:gethostname(),
                                    gen_event:notify(ditto, 
                                                     {pass, "ErlangBabe"}),
                                    gen_event:notify(ditto, {nick, NickVal}),
                                    gen_event:notify(ditto,{user, UserVal, 
                                                            Hostname, HostVal,
                                                            "Anonymous Coward"}),
                                    NextConnected = true
                            end;                
                        
                        Id -> 
                            io:format("Nothing: [~p] ~n", [Id]),
                            NextConnected = false
                    end,
                    wxDialog:destroy(Dialog)
            end,
            loop(Frame, Text, Log, NextConnected);


        #wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
            gen_event:stop(ditto),
            io:format("Bye.");

        #wx{id=?FRAME, event=#wxClose{type=close_window}} ->
            gen_event:stop(ditto),
            io:format("Bye.");
        
        {server, {_Prefix, Command, Args}} ->
            Line = Command ++ " "
                ++ lists:foldr(fun(X, Y) -> X ++ " " ++ Y end, "", Args),
            wxTextCtrl:writeText(Log, Line ++ "\n"),
            loop(Frame, Text, Log, Connected);
        
        Msg ->
            io:format("[DEBUG] Lost message: ~p~n", [Msg]),
            loop(Frame, Text, Log, Connected)
    end.

