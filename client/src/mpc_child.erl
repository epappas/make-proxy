-module(mpc_child).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {encrState, decrState, lsock, socket, remote}).

-include("../../common/socks_type.hrl").
-define(TIMEOUT, 1000 * 60 * 10).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSock]) ->
    {ok, Key} = application:get_env(make_proxy_client, key),
  {EncrState, DecrState} =
    case application:get_env(make_proxy_client, iv) of
        {ok, IV} ->
            {mp_crypto:init(Key, IV), mp_crypto:init(Key, IV)};
        _ ->
            {mp_crypto:init(Key), mp_crypto:init(Key)}
    end,
    {ok, #state{encrState=EncrState, decrState=DecrState, lsock=LSock}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


%% the first message send to this child
handle_info(timeout, #state{encrState=EncrState, lsock=LSock, socket=undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    mpc_sup:start_child(),
    case start_process(Socket, EncrState) of
        {ok, Remote, NewEncrState} ->
            inet:setopts(Socket, [{active, once}]),
            inet:setopts(Remote, [{active, once}]),
            {noreply, State#state{encrState=NewEncrState, socket=Socket, remote=Remote}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

%% send by OPT timeout
handle_info(timeout, #state{socket=Socket} = State) when is_port(Socket) ->
    {stop, timeout, State};

%% recv from client, and send to remote
handle_info({tcp, Socket, Request}, #state{encrState=EncrState, socket=Socket, remote=Remote} = State) ->
    {NewEncrState, EncrRequest} = mp_crypto:encrypt(EncrState, Request),
    case gen_tcp:send(Remote, EncrRequest) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{encrState=NewEncrState}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

%% recv from remote, and send back to client
handle_info({tcp, Socket, Response}, #state{decrState=DecrState, socket=Client, remote=Socket} = State) ->
    {NewDecrState, RealData} = mp_crypto:decrypt(DecrState, Response),
    case gen_tcp:send(Client, RealData) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{decrState=NewDecrState}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket, remote=Remote}) ->
    case is_port(Socket) of
        true -> gen_tcp:close(Socket);
        false -> ok
    end,

    case is_port(Remote) of
        true -> gen_tcp:close(Remote);
        false -> ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_process(port(), nonempty_string()) -> {ok, port()} |
                                                  {error, any()}.
start_process(Socket, EncrState) ->
    {ok, RemoteAddr} = application:get_env(make_proxy_client, remote_addr),
    {ok, RemotePort} = application:get_env(make_proxy_client, remote_port),
    {ok, LocalPort} = application:get_env(make_proxy_client, local_port),
    {ok, Addr} = inet:getaddr(RemoteAddr, inet),

    {ok, Target} = find_target(Socket),

    {NewEncrState, EncryptedTarget} = mp_crypto:encrypt(EncrState, Target),
    case gen_tcp:connect(Addr, RemotePort, [binary, {active, false}, {packet, 4}]) of
        {ok, RemoteSocket} ->
            ok = gen_tcp:send(RemoteSocket, EncryptedTarget),
            ok = gen_tcp:send(Socket, <<5, 0, 0, 1, <<0,0,0,0>>/binary, LocalPort:16>>),
            {ok, RemoteSocket, NewEncrState};
        {error, Error} ->
            {error, Error}
    end.


-spec find_target(port()) -> {ok, <<_:32, _:_*8>>}.
find_target(Socket) ->
    {ok, <<5:8, Nmethods:8>>} = gen_tcp:recv(Socket, 2),
    {ok, _Methods} = gen_tcp:recv(Socket, Nmethods),

    gen_tcp:send(Socket, <<5:8/integer, 0:8/integer>>),
    {ok, <<5:8, 1:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Socket, 4),

    case AType of
        ?IPV4 ->
            {ok, <<Address:32>>} = gen_tcp:recv(Socket, 4),
            {ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
            {ok, <<?IPV4, Port:16, Address:32>>};
        ?IPV6 ->
            {ok, <<Address:128>>} = gen_tcp:recv(Socket, 16),
            {ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
            {ok, <<?IPV6, Port:16, Address:128>>};
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Socket, 1),
            {ok, <<DomainBin/binary>>} = gen_tcp:recv(Socket, DomainLen),
            {ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
            {ok, <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>}
    end.

