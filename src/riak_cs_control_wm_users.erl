%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Resource to manage users.

-module(riak_cs_control_wm_users).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         post_is_create/2,
         create_path/2,
         from_json/2,
         to_json/2,
         routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {users=undefined,user=undefined}).

init([]) ->
    riak_cs_control_helpers:configure_s3_connection(),
    {ok, #context{users=undefined,user=undefined}}.

routes() ->
    [{["admin", "users"], ?MODULE, []}].

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc Extract key out of response from riak-cs.
extract_key_id(User) ->
    {struct, UserDetails} = User,
    binary_to_list(proplists:get_value(list_to_binary("key_id"), UserDetails)).

%% @doc Attempt to create the user if possible, and generate the path
%% using the key_id of the new user.
create_path(ReqData, Context) ->
    case maybe_create_user(ReqData, Context) of
        {true, NewContext} ->
            User = NewContext#context.user,
            KeyId = extract_key_id(User),
            Resource = "/users/" ++ KeyId,
            NewReqData = wrq:set_resp_header("Location", Resource, ReqData),
            {Resource, NewReqData, NewContext};
        {false, Context} ->
            {"/users", ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

%% @doc Handle user creation.
from_json(ReqData, Context) ->
    case maybe_create_user(ReqData, Context) of
        {true, NewContext} ->
            User = NewContext#context.user,
            Response = mochijson2:encode({struct, [{user, User}]}),
            NewReqData = wrq:set_resp_body(Response, ReqData),
            {true, NewReqData, NewContext};
        {false, Context} ->
            {{halt, 409}, ReqData, Context}
    end.

resource_exists(ReqData, Context) ->
    case maybe_retrieve_users(Context) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Attempt to create a user, and stick into the context for
%% serialization later.  Return 409 on any error for now.
maybe_create_user(ReqData, Context) ->
    case Context#context.user of
        undefined ->
            try
                Attributes = wrq:req_body(ReqData),
                NewAttributes = riak_cs_control_helpers:reencode_attributes(Attributes),
                {_Headers, Body} = riak_cs_control_session:put_user(NewAttributes),
                ParsedResponse = mochijson2:decode(Body),
                {true, Context#context{user=ParsedResponse}}
            catch
                error:_ ->
                    {false, Context}
            end;
        _User ->
            {true, Context}
    end.

%% @doc Attempt to retrieve the user or return an exception if it doesn't
%% exist.
maybe_retrieve_users(Context) ->
    case Context#context.users of
        undefined ->
            try
                Response = riak_cs_control_session:get_users(),
                ParsedResponse = parse_multipart_response(Response),
                {true, Context#context{users=ParsedResponse}}
            catch
                error:_ ->
                    {false, Context}
            end;
        _Users ->
            {true, Context}
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

%% @doc Return serialized users.
to_json(ReqData, Context) ->
    case maybe_retrieve_users(Context) of
        {true, NewContext} ->
            Users = NewContext#context.users,
            Response = mochijson2:encode({struct, [{users, Users}]}),
            {Response, ReqData, NewContext};
        {false, Context} ->
            Response = mochijson2:encode({struct, [{users, []}]}),
            {Response, ReqData, Context}
    end.

-ifdef(TEST).
-endif.
