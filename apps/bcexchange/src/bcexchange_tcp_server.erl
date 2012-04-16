-module(bcexchange_tcp_server).
-behaviour(gen_fsm).
-include_lib("bccompute/include/bccompute.hrl").
-export([start_link/1]).
-export([set_socket/2]).
-export([init/1, waiting_socket/3, connected/2, connected/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-record(state, {socket}).

start_link(SslOpts) ->
    gen_fsm:start_link(?MODULE, [], [SslOpts]).

init(_) ->
    {ok, waiting_socket, #state{}}.

set_socket(Pid, Socket) ->
    gen_fsm:sync_send_event(Pid, {set_socket, Socket}).

waiting_socket({set_socket, Socket}, _From, State) ->
    inet:setopts(Socket, [{active, once}]),
    {reply, ok, negotiate, State#state{socket=Socket}}.

connected(_Event, State) ->
    {next_state, connected, State}.

connected(_Event, _From, State) ->
    {reply, ok, connected, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({tcp_closed, Socket}, StateName, State=#state{socket=Socket}) ->
    lager:info("Connection closed in state ~p", [StateName]),
    {stop, normal, State};
handle_info({tcp_closed, _Socket}, StateName, State) ->
    {next_state, StateName, State};
handle_info({tcp_error, Socket, Reason}, _StateName, State=#state{socket=Socket}) ->
    lager:error("Connection closed unexpectedly: ~p", [Reason]),
    {stop, normal, State};
handle_info({tcp, Socket, [MsgType|MsgDataBin]}, _StateName, State=#state{socket=Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    NewState = handle_msg(MsgType, binary_to_term(MsgDataBin), State),
    {next_state, connected, NewState}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_msg(?BCC_MSG_CONFIG, MsgProps, State) ->
    io:format("config props: ~p~n", [MsgProps]).
