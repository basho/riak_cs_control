%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Resource to manage one user.

-module(riak_cs_control_wm_user).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
         forbidden/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         from_json/2,
         to_json/2,
         routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {user=undefined}).

%% @doc Initialize the resource.
init([]) ->
    {ok, #context{user=undefined}}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["users", key_id], ?MODULE, []}].

%% @doc Validate CSRF token.
forbidden(ReqData, Context) ->
    {riak_cs_control_security:is_protected(ReqData, Context), ReqData, Context}.

%% @doc Support retrieval and updates for a user.
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'PUT'], ReqData, Context}.

%% @doc Provide respones in JSON only.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Accept user updates in JSON only.
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

%% @doc Attempt to retrieve the user and store in the context if
%% possible.
maybe_retrieve_user(Context, KeyId) ->
    case Context#context.user of
        undefined ->
            case riak_cs_control_session:get_user(KeyId) of
                {ok, Response} ->
                    {true, Context#context{user=Response}};
                _ ->
                    {false, Context}
            end;
        _User ->
            {true, Context}
    end.

%% @doc Handle updating a particular user; return 404 on non-existent
%% user, 500 on general error, and 204 on proper update.
from_json(ReqData, Context) ->
    KeyId = key_id(ReqData),
    case maybe_retrieve_user(Context, KeyId) of
        {true, NewContext} ->
            Attributes = wrq:req_body(ReqData),
            NewAttributes = riak_cs_control_formatting:format_incoming_user(Attributes),
            case riak_cs_control_session:put_user(KeyId, NewAttributes) of
                {ok, _Response} ->
                    Resource = "/users/" ++ KeyId,
                    NewReqData = wrq:set_resp_header("Location", Resource, ReqData),
                    {{halt, 204}, NewReqData, NewContext};
                _ ->
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
            Response = mochijson2:encode({struct, [{user, User}]}),
            {Response, ReqData, NewContext};
        {false, Context} ->
            Response = mochijson2:encode({struct, []}),
            {Response, ReqData, Context}
    end.

-ifdef(TEST).
-endif.
