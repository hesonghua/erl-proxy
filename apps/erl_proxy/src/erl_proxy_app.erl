%%%-------------------------------------------------------------------
%% @doc erl_proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_proxy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rand:seed(exs1024),
    {ok, Pid} = erl_proxy_sup:start_link(),
    {ok, ProxyList} = application:get_env(proxy_list),
    lists:map(fun({Host, Port, MFA}) -> erl_proxy_sup:start_proxy(MFA, Host, Port) end, ProxyList),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
