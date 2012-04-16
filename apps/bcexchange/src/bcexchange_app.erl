-module(bcexchange_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case bcexchange_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, bcexchange_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(bcexchange_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(bcexchange_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(bcexchange, self()),
            %% Add routes to webmachine
            [ webmachine_router:add_route(R)
              || R <- lists:reverse(bcexchange_web:dispatch_table()) ],
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
