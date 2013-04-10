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

-type url() :: list().
-type request_type() :: multipart_get | get | put.
-type attributes() :: list().
-type response() :: list() | tuple().
-type body() :: list().

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
    case handle_request({multipart_get, "users"}) of
        {ok, Response} ->
            Users = riak_cs_control_formatting:format_users(Response),
            {reply, {ok, Users}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_user, KeyId}, _From, State) ->
    case handle_request({get, "user/" ++ KeyId}) of
        {ok, Response} ->
            User = riak_cs_control_formatting:format_user(Response),
            {reply, {ok, User}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({put_user, Attributes}, _From, State) ->
    case handle_request({put, "user", Attributes}) of
        {ok, Response} ->
            User = riak_cs_control_formatting:format_user(Response),
            {reply, {ok, User}, State};
        Error ->
            {reply, Error, State}
    end;

handle_call({put_user, KeyId, Attributes}, _From, State) ->
    case handle_request({put, "user/" ++ KeyId, Attributes}) of
        {ok, Response} ->
            User = riak_cs_control_formatting:format_user(Response),
            {reply, {ok, User}, State};
        Error ->
            {reply, Error, State}
    end;

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

%% @doc Perform a put request to Riak CS.
-spec put_request(url(), body()) -> {ok, response()} | {error, term()}.
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

%% @doc Perform a get request to Riak CS.
-spec get_request(url()) -> {ok, response()} | {error, term()}.
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

%% @doc Empty decoded JSON placeholder.
-spec empty_response() -> {struct, list()}.
empty_response() -> {struct, []}.

%% @doc Handle get/put requets.
-spec handle_request({request_type(), url()} |
                     {request_type(), url(), attributes()}) ->
    {ok, response()} | {error, term()}.
handle_request({multipart_get, Url}) ->
    case get_request(Url) of
        {ok, Content} ->
            {ok, riak_cs_control_multipart:parse_multipart_response(Content)};
        _ ->
            {ok, []}
    end;
handle_request({get, Url}) ->
    case get_request(Url) of
        {ok, Content} ->
            Body = proplists:get_value(content, Content),
            {ok, mochijson2:decode(Body)};
        _ ->
            {ok, empty_response()}
    end;
handle_request({put, Url, Body}) ->
    case put_request(Url, Body) of
        {ok, {_ResponseHeaders, ResponseBody}} ->
            {ok, mochijson2:decode(ResponseBody)};
        {error, Error} ->
            {error, Error}
    end.
