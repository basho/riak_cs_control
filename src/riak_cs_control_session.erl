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
    riak_cs_control_configuration:configure_s3_connection(),
    {ok, #state{}}.

handle_call(get_users, _From, State) ->
    Response = handle_request({multipart_get, "users"}),
    {reply, {ok, Response}, State};

handle_call({get_user, KeyId}, _From, State) ->
    Response = handle_request({get, "user/" ++ KeyId}),
    {reply, {ok, Response}, State};

handle_call({put_user, Attributes}, _From, State) ->
    Response = handle_request({put, "user", Attributes}),
    {reply, {ok, Response}, State};

handle_call({put_user, KeyId, Attributes}, _From, State) ->
    Response = handle_request({put, "user/" ++ KeyId, Attributes}),
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

%% @spec put_request(list(), list()) -> {ok, list()} | {error, list()}
%% @doc Perform a put request to Riak CS.
put_request(Url, Body) ->
    try
        Response = erlcloud_s3:put_object(
            riak_cs_control_configuration:administration_bucket_name(),
            Url,
            Body,
            [{return_response, true}],
            [{"content-type", "application/json"}]),
        {ok, Response}
    catch
        error:Reason -> {error, Reason}
    end.

%% @spec get_request(list()) -> {ok, list()} | {error, list()}
%% @doc Perform a get request to Riak CS.
get_request(Url) ->
    try
        Response = erlcloud_s3:get_object(
            riak_cs_control_configuration:administration_bucket_name(),
            Url,
            [{accept, "application/json"}]),
        {ok, Response}
    catch
        error:Reason -> {error, Reason}
    end.

%% @spec empty_response() -> {struct, list()}
%% @doc Empty decoded JSON placeholder.
empty_response() -> {struct, []}.

%% @spec handle_request({multipart_get, list()}) -> list()
%% @doc Handle multipart get request.
handle_request({multipart_get, Url}) ->
    case get_request(Url) of
        {ok, Content} ->
            riak_cs_control_multipart:parse_multipart_response(Content);
        {error, Reason} ->
            lager:info("Multipart request failed: ~s", [Reason]),
            []
    end;

%% @spec handle_request({get, list()}) -> {struct, list()}
%% @doc Handle get request.
handle_request({get, Url}) ->
    case get_request(Url) of
        {ok, Content} ->
            Body = proplists:get_value(content, Content),
            mochijson2:decode(Body);
        {error, Reason} ->
            lager:info("Get request failed: ~s", [Reason]),
            empty_response()
    end;

%% @spec handle_request({put, list(), list()}) -> {struct, list()}
%% @doc Handle put request.
handle_request({put, Url, Body}) ->
    case put_request(Url, Body) of
        {ok, {_Headers, Body}} ->
            mochijson2:decode(Body);
        {error, Reason} ->
            lager:info("Put request failed: ~s", [Reason]),
            empty_response()
    end.
