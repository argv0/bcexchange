%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(bcexchange_web).

-export([dispatch_table/0]).

dispatch_table() ->
    [
     {["bcexchange"], bcexchange_wm_service, []}
    ].
