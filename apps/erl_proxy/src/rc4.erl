-module(rc4).
-export([init/0, new/1, update/2]).
-define(APPNAME, erl_proxy).
-define(LIBNAME, rc4).
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

new(_Key) ->
    "not implemented".

update(_RC4, _Data) ->
    "not implemented".