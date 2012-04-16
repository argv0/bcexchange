-module(bcexchange_listener_sup).
-behaviour(supervisor).

%% beahvior functions
-export([start_link/0,
         init/1,
         start_listener/1
        ]).

-define(CHILD(I,Type), {I,{I,start_link,[]},permanent,brutal_kill,Type,[I]}).

%% begins the supervisor, init/1 will be called
start_link () ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @private
init ([]) ->
    {ok,{{simple_one_for_one,10,10},
         [?CHILD(bcexchange_listener,worker)
         ]}}.

%% start a listener process
start_listener(PortOffset) ->
    supervisor:start_child(?MODULE,[PortOffset]).
