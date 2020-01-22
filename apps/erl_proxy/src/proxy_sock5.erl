-module(proxy_sock5).
-export([start_link/1, loop/1]).
-include("common.hdr").

-record(state, {options, downsock, upsock, downbuf, upbuf, downkey=undefined, idown=0, odown=0, upkey=undefined, iup=0, oup = 0}).

sock_send(Socket, Bin, S=#state{downsock=Socket, downkey=Key, odown=Index}) ->
    {Bin1, Index1} = utils:encode(Bin, Key, Index),
    gen_tcp:send(Socket, Bin1),
    S#state{odown = Index1};

sock_send(Socket, Bin, S=#state{upsock=Socket, upkey=Key, oup=Index}) ->
    {Bin1, Index1} = utils:encode(Bin, Key, Index),
    gen_tcp:send(Socket, Bin1),
    S#state{oup = Index1}.

sock_recv(Socket, Bin, S=#state{downsock=Socket, downkey=Key, idown=Index}) ->
    {Bin1, Index1} = utils:decode(Bin, Key, Index),
    {Bin1, S#state{idown=Index1}};

sock_recv(Socket, Bin, S=#state{upsock=Socket, upkey=Key, iup=Index}) ->
    {Bin1, Index1} = utils:decode(Bin, Key, Index),
    {Bin1, S#state{iup=Index1}}.

start_link(Options) ->
    Pid = spawn_link(?MODULE, loop, [#state{options=Options}]),
    {ok, Pid}.

loop(S) ->
    receive
        {go, Socket} ->
            inet:setopts(Socket, [{active, once}]),
            handle(version, S#state{downsock=Socket, downbuf= <<>> })
    end.

handle(version, S = #state{downsock=Socket, downbuf=Buffer, options=Options}) ->
    {ok, Bin} = utils:read_at_least(Socket, 2, Buffer),
    <<5:8, Nmethods:8, Rest/binary>> = Bin,
    {ok, <<Methods:Nmethods/binary, Rest1/binary>>} = utils:read_at_least(Socket, Nmethods, Rest),
    error_logger:info_msg("Nmethods=~p, Methods=~p~n", [Nmethods, Methods]),
    LocalAuth = proplists:get_value(auth, Options, {0}),
    <<Method:8, _/binary>> = Methods,
    handle({Method, LocalAuth}, S#state{downbuf=Rest1});


handle({16#7c, {16#7c, Password}}, S=#state{downsock=Socket, downbuf=Buffer}) ->
    {A, B, C, D} = {rand:uniform(255), rand:uniform(255),rand:uniform(255),rand:uniform(255)},
    gen_tcp:send(Socket, <<5:8, 16#7c, A:8, B:8, C:8, D:8>>),
    {MD5, Key} = utils:calc_md5_key(list_to_binary(Password), <<A:8,B:8,C:8,D:8>>),
    {ok, <<MD5:16/binary, Rest/binary>>} = utils:read_at_least(Socket, 16, Buffer),
    gen_tcp:send(Socket, <<5:8, 0:8>>),
    handle(request, S#state{downkey=binary_to_list(Key), downbuf=Rest});

handle({2, {2, Username, Password}}, S=#state{downsock=Socket, downbuf=Buffer}) ->
    gen_tcp:send(Socket, <<5:8, 2:8>>),
    {UBin, PBin} = {list_to_binary(Username), list_to_binary(Password)},
    {ok, <<VER:8, ULEN:8, Rest/binary>>} = utils:read_at_least(Socket, 2, Buffer),
    {ok, <<UBin:ULEN/binary, PLEN:8, Rest1/binary>>} = utils:read_at_least(Socket, ULEN + 1, Rest),
    {ok, <<PBin:PLEN/binary, Rest2/binary>>} = utils:read_at_least(Socket, PLEN, Rest1),
    gen_tcp:send(Socket, <<VER:8, 0:8>>),
    handle(request, S#state{downbuf=Rest2});

handle({0, {0}}, S=#state{downsock=Socket}) ->
    gen_tcp:send(Socket, <<5:8, 0:8>>),
    handle(request, S);

handle(request, S1 = #state{downsock=Socket, downbuf=Buffer, options=Options}) ->
    receive
        {tcp, Socket, Data1} ->
            Forward = proplists:get_value(forward, Options, undefined),
            inet:setopts(Socket, [{active, once}]),
            {Data, S} = sock_recv(Socket, Data1, S1),
            Bin = list_to_binary([Buffer, Data]),
            case Bin of
                <<5:8, Cmd:8, _:8, 3:8, Hostlen:8, Host:Hostlen/binary, Port:?BIG_WORD, Rest/binary>> ->
                    handle({url, Forward, {Cmd, binary_to_list(Host), Port}}, S#state{downbuf=Rest});
                <<5:8, Cmd:8, _:8, 1:8, IP:?BIG_DWORD, Port:?BIG_WORD, Rest/binary>> ->
                    <<P1:8, P2:8, P3:8, P4:8>> = <<IP:?BIG_DWORD>>,
                    Host = lists:flatten(io_lib:format("~p.~p.~p.~p", [P1, P2, P3, P4])),
                    handle({url, Forward, {Cmd, Host, Port}}, S#state{downbuf=Rest});
                _ -> handle(request, S#state{downbuf=Bin})
            end;
        {tcp_closed, Socket} -> ok
    end;

handle({url, undefined, {Cmd, Host, Port}}, S1=#state{downsock=Socket, downbuf=Buffer}) ->
    error_logger:info_msg("handle Cmd:~p Host:~p Port:~p Buffer:~p~n", [Cmd, Host, Port, Buffer]),
    case gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, 0}], 5000) of
        {ok, UpSock} ->
            error_logger:info_msg("connected:  ~p:~p~n", [Host, Port]),
            handle({url, response}, S1#state{upsock=UpSock, upbuf=[]});
        {error, Reason} ->
            error_logger:info_msg("connect error: ~p~n", [Reason]),
            sock_send(Socket, <<5:8, 0:8, 0:8, 0:8, 0:32, 0:?BIG_WORD>>, S1)
    end;

handle({url, Forward = {ForwardHost, ForwardPort, Auth}, {_, Host, Port}}, S1=#state{downbuf=Buffer}) ->
    error_logger:info_msg("handle forward:~p Buffer:~p~n", [Forward, Buffer]),
    {ok, UpSock, Key, IKey, OKey} = sock5:connect({ForwardHost, ForwardPort}, {Host, Port}, Auth),
    handle({url, response}, S1#state{upsock=UpSock, upbuf=[], upkey=Key, iup=IKey, oup=OKey});

handle({url, response}, S=#state{downsock=Socket}) ->
    S1 = sock_send(Socket, <<5:8, 0:8, 0:8, 1:8, 0:32, 0:16>>, S),
    handle(transfer, S1);

handle(transfer, S1=#state{downsock=DownSock, upsock=UpSock}) ->
    receive
        {tcp, DownSock, DownBin1} ->
            inet:setopts(DownSock, [{active, once}]),
            {DownBin, S2} = sock_recv(DownSock, DownBin1, S1),
            S = sock_send(UpSock, DownBin, S2),
            handle(transfer, S);
        {tcp, UpSock, UpBin1} ->
            inet:setopts(UpSock, [{active, once}]),
            {UpBin, S2} = sock_recv(UpSock, UpBin1, S1),
            S = sock_send(DownSock, UpBin, S2),
            handle(transfer, S);
        {tcp_closed, DownSock} ->
            error_logger:info_msg("downstream closed.~n", []);
        {tcp_closed, UpSock} ->
            error_logger:info_msg("upstream closed.~n", [])
    end.
