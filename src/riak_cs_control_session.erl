%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Generic server for performing Riak CS operations.

-module(riak_cs_control_session).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_users/0,
         get_user/1,
         put_user/1,
         put_user/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_users() ->
    gen_server:call(?MODULE, get_users, infinity).

get_user(KeyId) ->
    gen_server:call(?MODULE, {get_user, KeyId}, infinity).

put_user(Attributes) ->
    gen_server:call(?MODULE, {put_user, Attributes}, infinity).

put_user(KeyId, Attributes) ->
    gen_server:call(?MODULE, {put_user, KeyId, Attributes}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_users, _From, State) ->
    Response = get_request("users"),
    {reply, {ok, Response}, State};

handle_call({get_user, KeyId}, _From, State) ->
    Response = get_request("user/" ++ KeyId),
    {reply, {ok, Response}, State};

handle_call({put_user, Attributes}, _From, State) ->
    Response = put_request("user", Attributes),
    {reply, {ok, Response}, State};

handle_call({put_user, KeyId, Attributes}, _From, State) ->
    Response = put_request("user/" ++ KeyId, Attributes),
    {reply, {ok, Response}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

put_request(Url, Body) ->
    erlcloud_s3:put_object(
        riak_cs_control_helpers:administration_bucket_name(),
        Url,
        Body,
        [{return_response, true}],
        [{"content-type", "application/json"}]).

get_request(Url) ->
    erlcloud_s3:get_object(
        riak_cs_control_helpers:administration_bucket_name(),
        Url,
        [{accept, "application/json"}]).
