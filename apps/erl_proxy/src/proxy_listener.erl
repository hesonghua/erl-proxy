-module(proxy_listener).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-include("common.hdr").
-record(state, {lsock, ref, mfa}).

start_link(MFA, Host, Port) ->
    gen_server:start_link(?MODULE, {MFA, Host, Port}, []).

init({MFA, Host, Port}) ->
    process_flag(trap_exit, true),
    self() ! {start_listener, Host, Port},
    {ok, #state{mfa = MFA}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(accept, State) ->
    accept(State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_listener, Host, Port}, State) ->
    error_logger:info_msg("start_listener ~p:~p~n", [Host, Port]),
    {ok, IpAddr} = inet:parse_address(Host),
    {ok, ListenSock} = gen_tcp:listen(Port, ?TCP_OPTIONS ++ [{ip, IpAddr}]),
%    {ok, ListenSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    error_logger:info_msg("gen_tcp:listen success~n", []),
    gen_server:cast(self(), accept),
    {noreply, State#state{lsock = ListenSock}};

handle_info({inet_async, LSock, Ref, {ok, Sock}}, State = #state{lsock=LSock, ref=Ref, mfa=MFA}) ->
    case set_sockopt(LSock, Sock) of
        ok -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
    end,
    start_client(Sock, MFA),
    accept(State);

handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{lsock=LSock, ref=Ref}) ->
    {stop, normal, State};

handle_info(Info, State) ->
    error_logger:info_msg("~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.lsock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

accept(State = #state{lsock=LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.

set_sockopt(LSock, Sock) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Sock, Opts) of
                ok    -> ok;
                Error -> 
                    gen_tcp:close(Sock),
                    Error
            end;
        Error ->
            gen_tcp:close(Sock),
            Error
    end.

start_client(Sock, [M, F, A]) ->
    error_logger:info_msg("start_client ~p ~p ~n", [M, F]),
    {ok, Pid} = apply(M, F, A),
    ok = gen_tcp:controlling_process(Sock, Pid),
    Pid ! {go, Sock}.
