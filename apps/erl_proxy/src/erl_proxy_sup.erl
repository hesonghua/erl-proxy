-module(erl_proxy_sup).
-behavior(supervisor).
-export([start_link/0, stop/0, start_proxy/3, stop_proxy/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    ok.

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{proxy_sup, {proxy_sup, start_link, []},
            temporary, brutal_kill, supervisor, [proxy_sup]}]}}.

start_proxy(MFA, Host, Port) ->
    supervisor:start_child(?MODULE, [MFA, Host, Port]).

stop_proxy({}) ->
    ok.
