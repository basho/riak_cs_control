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
    riak_cs_control_helpers:configure_s3_connection(),
    {ok, #state{}}.

handle_call(get_users, _From, State) ->
    Response = case get_request("users") of
        {ok, Content} ->
            parse_multipart_response(Content);
        _ ->
            []
    end,
    {reply, {ok, Response}, State};

handle_call({get_user, KeyId}, _From, State) ->
    Response = case get_request("user/" ++ KeyId) of
        {ok, Content} ->
            User = proplists:get_value(content, Content),
            mochijson2:decode(User);
        _ ->
            {struct, []}
    end,
    {reply, {ok, Response}, State};

handle_call({put_user, Attributes}, _From, State) ->
    Response = case put_request("user", Attributes) of
        {ok, {_Headers, Body}} ->
            mochijson2:decode(Body);
        _ ->
            {struct, []}
    end,
    {reply, {ok, Response}, State};

handle_call({put_user, KeyId, Attributes}, _From, State) ->
    Response = case put_request("user/" ++ KeyId, Attributes) of
        {ok, {_Headers, Body}} ->
            mochijson2:decode(Body);
        _ ->
            {struct, []}
    end,
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

%% @doc Perform a put request to Riak CS.
put_request(Url, Body) ->
    try
        Response = erlcloud_s3:put_object(
            riak_cs_control_helpers:administration_bucket_name(),
            Url,
            Body,
            [{return_response, true}],
            [{"content-type", "application/json"}]),
        {ok, Response}
    catch
        error:Reason -> {error, Reason}
    end.

%% @doc Perform a get request to Riak CS.
get_request(Url) ->
    try
        Response = erlcloud_s3:get_object(
            riak_cs_control_helpers:administration_bucket_name(),
            Url,
            [{accept, "application/json"}]),
        {ok, Response}
    catch
        error:Reason -> {error, Reason}
    end.

%% @doc Strip leading carriage return and line feed so it's valid
%% multipart/mixed (this is the body seperator which erlcloud is not
%% properly handling).
reformat_multipart(Content) ->
    list_to_binary(string:substr(binary_to_list(Content), 3)).

%% @doc Extract boundary from multipart header.
extract_boundary(ContentType) ->
    string:substr(ContentType, string:str(ContentType, "boundary=") +
                  length("boundary=")).

%% @doc Given a series of multipart documents, extract just body out and
%% parse based on content type.
parse_bodies(Parts) ->
    [mochijson2:decode(Body) || {_Name, {_Params, _Headers}, Body} <- Parts].

%% @doc Given a series of parsed JSON documents, merge.
merge_bodies(Bodies) ->
    lists:flatten(Bodies).

%% @doc Parse multipart user response.
parse_multipart_response(Response) ->
    Content = proplists:get_value(content, Response),
    ContentType = proplists:get_value(content_type, Response),
    ModifiedContent = reformat_multipart(Content),
    Boundary = extract_boundary(ContentType),
    Parts = webmachine_multipart:get_all_parts(ModifiedContent, Boundary),
    Bodies = parse_bodies(Parts),
    merge_bodies(Bodies).
