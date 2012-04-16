-module(bcexchange_listener).
-behavior(gen_nb_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([sock_opts/0, new_connection/2]).
-record(state, {
          portnum :: integer(),
          ssl_opts :: list()
         }).

start_link(PortOffset) ->
    gen_nb_server:start_link(?MODULE, "127.0.0.1", 51200+PortOffset, [PortOffset]).

init([PortOffset]) ->
    PortNum = 51200 + PortOffset,
    {ok, #state{portnum=PortNum}}.

sock_opts() -> [binary, {header, 1},
                {packet, 4}, {reuseaddr, true}, {backlog, 64}].

handle_call(portnum, _From, State=#state{portnum=P}) ->
    {reply, {ok, P}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

new_connection(Socket, State = #state{ssl_opts = SslOpts}) ->
    {ok, Pid} = bcexchange_tcp_server:start_link(SslOpts),
    gen_tcp:controlling_process(Socket, Pid),
    ok = bcexchange_tcp_server:set_socket(Pid, Socket),
    {ok, State}.
