-module(sock5).
-export([connect/3]).
-include("common.hdr").

connect({Host,Port}, Target, Auth) ->
    case gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, 0}], 5000) of
        {ok, Socket} ->
            handshake(Socket, Target, Auth);
        {error, Reason} -> {error, Reason}
    end.

handshake(Socket, Target, {0}) ->
    gen_tcp:send(Socket, <<5:8, 1:8, 0:8>>),
    {ok, Bin} = utils:read_at_least(Socket, 2, <<>>),
    <<5:8, 0:8>> = Bin,
    request(Socket, Target, undefined, undefined);

handshake(Socket, Target, {16#7c, Password}) ->
    gen_tcp:send(Socket, <<5:8, 1:8, 16#7c:8>>),
    {ok, Bin} = utils:read_at_least(Socket, 2, <<>>),
    case Bin of
        <<5:8, 0:8>> ->
            request(Socket, Target, undefined, undefined);
        <<5:8, 16#7c:8, Rest/binary>> ->
            auth(Socket, Target, Rest, {16#7c, Password})
    end;

handshake(Socket, Target, {2, Username, Password}) ->
    gen_tcp:send(Socket, <<5:8, 1:8, 2:8>>),
    {ok, Bin} = utils:read_at_least(Socket, 2, <<>>),
    case Bin of
        <<5:8, 0:8>> ->
            request(Socket, Target, undefined, undefined);
        <<5:8, 2:8, Rest/binary>> ->
            auth(Socket, Target, Rest, {2, Username, Password})
    end.

auth(Socket, Target, Buffer, {16#7c, Password}) ->
    {ok, Bin} = utils:read_at_least(Socket, 4, Buffer),
    <<RandBin:4/binary, Rest/binary>> = Bin,
    {MD5, Key} = utils:calc_md5_key(list_to_binary(Password), RandBin),
    gen_tcp:send(Socket, MD5),
    {ok, Bin1} = utils:read_at_least(Socket, 2, Rest),
    <<5:8, 0:8>> = Bin1,
    request(Socket, Target, mycrypto:simple_new(Key), mycrypto:simple_new(Key));

auth(Socket, Target, Buffer, {2, Username, Password}) ->
    {UBin, PBin} = {list_to_binary(Username), list_to_binary(Password)},
    {ULEN, PLEN} = {size(UBin), size(PBin)},
    SendBin = <<1:8, ULEN:8, UBin:ULEN/binary, PLEN:8, PBin:PLEN/binary>>,
    gen_tcp:send(Socket, SendBin),
    {ok, <<_:8, 0:8, _/binary>>} = utils:read_at_least(Socket, 2, Buffer),
    request(Socket, Target, undefined, undefined).

request(Socket, {Host, Port}, InEnc, OutEnc) ->
    HostBin = list_to_binary(Host),
    HostLen = size(HostBin),
    SendBin = <<5:8, 1:8, 0:8, 3:8, HostLen:8, HostBin/binary, Port:?BIG_WORD>>,
    {SendBin1, OutEnc1} = utils:encode(SendBin, OutEnc),
    gen_tcp:send(Socket, SendBin1),
    {ok, Bin} = utils:read_at_least(Socket, 10, <<>>),
    {Bin1, InEnc1} = utils:decode(Bin, InEnc),
    <<5:8, 0:8, _:8/binary>> = Bin1,
    {ok, Socket, InEnc1, OutEnc1}.