-module(proxy_sup).
-behavior(supervisor).
-export([start_link/3]).
-export([init/1]).

start_link(MFA, Host, Port) ->
	error_logger:info_msg("proxy_sup(~p:~p)~n", [Host, Port]),
    supervisor:start_link(?MODULE, {MFA, Host, Port}).

init({MFA, Host, Port}) ->
    MaxRestart = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestart, MaxTime},
          [{listener,
            {proxy_listener, start_link, [MFA, Host, Port]},
            permanent,
            5000,
            worker,
            [proxy_listener]}]}}.

