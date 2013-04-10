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
         forbidden/2,
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

%% @doc Initialize the resource.
init([]) ->
    {ok, #context{users=undefined,user=undefined}}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["users"], ?MODULE, []}].

%% @doc Validate CSRF token.
forbidden(ReqData, Context) ->
    {riak_cs_control_security:is_protected(ReqData, Context), ReqData, Context}.

%% @doc Support retrieval and creation of users.
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'POST'], ReqData, Context}.

%% @doc Allow POST request to create user.
post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

%% @doc Extract key out of response from riak-cs.
-spec extract_key_id({term(), list()}) -> list().
extract_key_id(User) ->
    {struct, UserDetails} = User,
    KeyId = proplists:get_value(key_id, UserDetails, <<"">>),
    binary_to_list(KeyId).

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

%% @doc Provide respones in JSON only.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% @doc Accept user data in JSON only.
content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

%% @doc Accept user input, attempt to create user and return 409 if we
%% are unable to.
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

%% @doc Return true if we were able to retrieve the user.
resource_exists(ReqData, Context) ->
    case maybe_retrieve_users(Context) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Attempt to create user and stash in the context if possible.
maybe_create_user(ReqData, Context) ->
    case Context#context.user of
        undefined ->
            Attributes = wrq:req_body(ReqData),
            NewAttributes = riak_cs_control_formatting:format_incoming_user(Attributes),
            case riak_cs_control_session:put_user(NewAttributes) of
                {ok, Response} ->
                    {true, Context#context{user=Response}};
                _ ->
                    {false, Context}
            end;
        _User ->
            {true, Context}
    end.

%% @doc Attempt to retrieve the users and store in the context if
%% possible.
maybe_retrieve_users(Context) ->
    case Context#context.users of
        undefined ->
            case riak_cs_control_session:get_users() of
                {ok, Response} ->
                    {true, Context#context{users=Response}};
                _ ->
                    {false, Context}
            end;
        _Users ->
            {true, Context}
    end.

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
