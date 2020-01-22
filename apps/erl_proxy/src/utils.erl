-module(utils).
-export([calc_md5_key/2, encode/3, decode/3, zip_roll/3, read_at_least/3]).

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

zip_roll(X, K, 0, KS) -> zip_roll(X, K, KS);
zip_roll(X, K, N, []) -> zip_roll(X, K, N, K);
zip_roll(X, K, N, [_|KS]=K1) ->
    N1 = N rem length(K),
    case N1 == N of
        true -> zip_roll(X, K, N-1, KS);
        false -> zip_roll(X, K, N1, K1)
    end.

zip_roll(X, K, N) when is_integer(N) -> zip_roll(X, K, N, []);
zip_roll([], _, _) -> [];
zip_roll(X, K, []) -> zip_roll(X, K, K);
zip_roll([X|XS], K, [K1|KS]) -> [{X, K1} | zip_roll(XS, K, KS)].

decode(Bin, undefined, IKey) ->
    {Bin, IKey};

decode(Bin, Key, IKey) ->
    L = binary_to_list(Bin),
    L1 = zip_roll(L, Key, IKey),
    Out = [ D bxor K || {D, K} <- L1 ],
    {list_to_binary(Out), IKey + length(L)}.

encode(Bin, undefined, OKey) ->
    {Bin, OKey};

encode(Bin, Key, OKey) ->
    L = binary_to_list(Bin),
    L1 = zip_roll(L, Key, OKey),
    Out = [ D bxor K || {D, K} <- L1 ],
    {list_to_binary(Out), OKey + length(L)}.