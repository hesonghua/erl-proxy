-module(mycrypto).
-export([init/0, rc4_new/1, rc4_update/2, simple_new/1, simple_update/2]).
-define(APPNAME, erl_proxy).
-define(LIBNAME, mycrypto).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

rc4_new(_Key) ->
    "not implemented".

rc4_update(_RC4, _Data) ->
    "not implemented".

simple_new(_Key) ->
    "not implemented".

simple_update(_RC4, _Data) ->
    "not implemented".