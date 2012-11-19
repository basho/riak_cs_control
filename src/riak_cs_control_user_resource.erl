%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Resource to manage one user.

-module(riak_cs_control_user_resource).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         from_json/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {user=undefined}).

init([]) ->
    riak_cs_control_helpers:configure_s3_connection(),
    {ok, #context{user=undefined}}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

%% @doc Extract key_id out of the request.
key_id(ReqData) ->
    wrq:path_info(key_id, ReqData).

%% @doc Eagerly fetch user and store in the context; conditionally return
%% on the user existing.
resource_exists(ReqData, Context) ->
    case maybe_retrieve_user(Context, key_id(ReqData)) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Attempt to retrieve the user or return an exception if it doesn't
%% exist. On all errors from riak-cs, return 404 for now.
maybe_retrieve_user(Context, KeyId) ->
    case Context#context.user of
        undefined ->
            try
                Response = retrieve_user(KeyId),
                RawUser = proplists:get_value(content, Response),
                {struct, User} = mochijson2:decode(RawUser),
                {true, Context#context{user=User}}
            catch
                error:_ ->
                    {false, Context}
            end;
        _User ->
            {true, Context}
    end.

%% @doc Retrieve user object.
retrieve_user(KeyId) ->
    erlcloud_s3:get_object(
        riak_cs_control_helpers:administration_bucket_name(),
        "user/" ++ KeyId,
        [{accept, "application/json"}]).

%% @doc Store user object.
store_user(KeyId, Attributes) ->
    erlcloud_s3:put_object(
        riak_cs_control_helpers:administration_bucket_name(),
        "user/" ++ KeyId,
        Attributes,
        [{return_response, true}],
        [{"content-type", "application/json"}]).

%% @doc Handle updates on a per user basis.
from_json(ReqData, Context) ->
    KeyId = key_id(ReqData),
    case maybe_retrieve_user(Context, KeyId) of
        {true, NewContext} ->
            try
                Attributes = wrq:req_body(ReqData),
                {_Headers, _Body} = store_user(KeyId, Attributes),
                Resource = "/users/" ++ KeyId,
                NewReqData = wrq:set_resp_header("Location", Resource, ReqData),
                {{halt, 204}, NewReqData, NewContext}
            catch
                error:_ ->
                    {{halt, 500}, ReqData, Context}
            end;
        {false, Context} ->
            {{halt, 404}, ReqData, Context}
    end.

%% @doc Return serialized user.
to_json(ReqData, Context) ->
    case maybe_retrieve_user(Context, key_id(ReqData)) of
        {true, NewContext} ->
            User = NewContext#context.user,
            Response = mochijson2:encode({struct, [{user, {struct, User}}]}),
            {Response, ReqData, NewContext};
        {false, Context} ->
            Response = mochijson2:encode({struct, []}),
            {Response, ReqData, Context}
    end.

-ifdef(TEST).
-endif.
