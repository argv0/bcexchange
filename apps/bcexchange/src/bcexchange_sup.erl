-module(bcexchange_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    case application:get_env(bcexchange, bc_ip) of
        {ok, Ip} ->
            ok;
        undefined ->
            Ip = "0.0.0.0"
    end,
    case application:get_env(bcexchange, bc_port) of
        {ok, Port} ->
            ok;
        undefined ->
            Port = 8043
    end,
    %% Create child specifications
    WebConfig1 = [
                 {dispatch, bcexchange_web:dispatch_table()},
                 {ip, Ip},
                 {port, Port},
                 {nodelay, true},
                 {log_dir, "log"}],
    case application:get_env(bcexchange, ssl) of

        {ok, SSLOpts} ->
            WebConfig = WebConfig1 ++ [{ssl, true},
                                       {ssl_opts, SSLOpts}];
        undefined ->
            WebConfig = WebConfig1
    end,
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    ListenerSup = {bcexchange_listener_sup,
                   {bcexchange_listener_sup, start_link, []},
           permanent, 5000, supervisor, dynamic},
    VMaster = { bcexchange_vnode_master,
                  {riak_core_vnode_master, start_link, [bcexchange_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    {ok, {RiakCWorkers, RiakCMaxOverflow}} = application:get_env(riak_moss, riakc_pool),
    RiakCStop = fun(Worker) -> riak_moss_riakc_pool_worker:stop(Worker) end,
    RiakCPool = {riakc_pool,
                 {poolboy, start_link, [[{name, {local, riakc_pool}},
                                         {worker_module, riak_moss_riakc_pool_worker},
                                         {size, RiakCWorkers},
                                         {max_overflow, RiakCMaxOverflow},
                                         {stop_fun, RiakCStop}]]},
                 permanent, 5000, worker, [poolboy]},

    { ok,
        { {one_for_one, 5, 10},
          [RiakCPool, ListenerSup, VMaster, Web]}}.
