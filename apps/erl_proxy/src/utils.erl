-module(utils).
-export([calc_md5_key/2, encode/2, decode/2, read_at_least/3, for/3, for/4]).

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

calc_md5_key(PassBin, RandBin) ->
    MD5 = erlang:md5(<<PassBin/binary, RandBin/binary>>),
    InvPass = << <<bnot(X)>> || <<X>> <= PassBin>>,
    Key = erlang:md5(<<InvPass/binary, RandBin/binary>>),
    {MD5, Key}.

read_at_least(Socket, N, Buffer) ->
    case size(Buffer) >= N of
        true -> {ok, Buffer};
        false ->
            {ok, Buffer1} = read(Socket, Buffer),
            read_at_least(Socket, N, Buffer1)
    end.

read(Socket, Buffer) ->
    receive
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),
            {ok, <<Buffer/binary, Data/binary>>};
        {tcp_closed, Socket} ->
            {error, Buffer}
    end.

decode(Bin, undefined) ->
    {Bin, undefined};

decode(Bin, Enc) ->
    {Enc1, Bin1} = mycrypto:simple_update(Enc, Bin),
    {Bin1, Enc1}.

encode(Bin, undefined) ->
    {Bin, undefined};

encode(Bin, Enc) ->
    {Enc1, Bin1} = mycrypto:simple_update(Enc, Bin),
    {Bin1, Enc1}.
