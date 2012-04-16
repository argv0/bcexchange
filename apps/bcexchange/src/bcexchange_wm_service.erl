%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------
-module(bcexchange_wm_service).
-export([init/1,
         service_available/2,
         forbidden/2,
         content_types_provided/2,
         malformed_request/2,
         to_json/2,
         allowed_methods/2,
         finish_request/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(_Config) ->
    {ok, undefined}.

-spec service_available(term(), term()) -> {true, term(), term()}.
service_available(RD, Ctx) ->
    {true, RD, Ctx}.

-spec malformed_request(term(), term()) -> {false, term(), term()}.
malformed_request(RD, Ctx) ->
    {false, RD, Ctx}.

forbidden(RD, Ctx) ->
    {false, RD, Ctx}.

%% @doc Get the list of methods this resource supports.
-spec allowed_methods(term(), term()) -> {[atom()], term(), term()}.
allowed_methods(RD, Ctx) ->
    {['GET'], RD, Ctx}.

-spec content_types_provided(term(), term()) ->
    {[{string(), atom()}], term(), term()}.
content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

-spec to_json(term(), term()) -> {iolist(), term(), term()}.
to_json(RD, Ctx) ->
    {ok, R} = riak_core_ring_manager:get_my_ring(),
    {mochijson2:encode(riak_core_ring:my_indices(R)), RD, Ctx}.

finish_request(RD, Ctx) ->
    {true, RD, Ctx}.
