%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Resource to manage users.

-module(riak_cs_control_wm_disk_usage).
-author('Dmitri Zagidulin <dzagidulin@basho.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
         % forbidden/2,
         allowed_methods/2,
         content_types_provided/2,
         % content_types_accepted/2,
         % resource_exists/2,
         to_json/2,
         routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {disk_usage=undefined}).

%% @doc Initialize the resource.
init([]) ->
    {ok, #context{disk_usage=undefined}}.

%% @doc Return the routes this module should respond to.
routes() ->
    [{["disk_usage"], ?MODULE, []}].

%% @doc Validate CSRF token.
% forbidden(ReqData, Context) ->
%     {riak_cs_control_security:is_protected(ReqData, Context), ReqData, Context}.

%% @doc Support retrieval of disk usage stats
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

%% @doc Extract key out of response from riak-cs.
% -spec extract_key_id({term(), list()}) -> list().
% extract_key_id(User) ->
%     {struct, UserDetails} = User,
%     KeyId = proplists:get_value(key_id, UserDetails, <<"">>),
%     binary_to_list(KeyId).

%% @doc Attempt to create the user if possible, and generate the path
%% using the key_id of the new user.
% create_path(ReqData, Context) ->
%     case maybe_create_user(ReqData, Context) of
%         {true, NewContext} ->
%             User = NewContext#context.user,
%             KeyId = extract_key_id(User),
%             Resource = "/users/" ++ KeyId,
%             NewReqData = wrq:set_resp_header("Location", Resource, ReqData),
%             {Resource, NewReqData, NewContext};
%         {false, Context} ->
%             {"/users", ReqData, Context}
%     end.

%% @doc Provide respones in JSON only.
content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

% %% @doc Return true if we were able to retrieve the user.
% resource_exists(ReqData, Context) ->
%     case maybe_retrieve_usage(Context) of
%         {true, NewContext} ->
%             {true, ReqData, NewContext};
%         {false, Context} ->
%             {false, ReqData, Context}
%     end.

%% @doc Attempt to retrieve the users and store in the context if
%% possible.
maybe_retrieve_usage(Context) ->
    case Context#context.disk_usage of
        undefined ->
            case riak_cs_control_cs_api:get_cs_disk_usage() of
                {ok, Response} ->
                    {true, Context#context{disk_usage=Response}};
                _ ->
                    {false, Context}
            end;
        _Users ->
            {true, Context}
    end.

%% @doc Return serialized users.
to_json(ReqData, Context) ->
    case maybe_retrieve_usage(Context) of
        {true, NewContext} ->
            DiskUsage = NewContext#context.disk_usage,
            Response = mochijson2:encode({struct, [{disk_usage, DiskUsage}]}),
            {Response, ReqData, NewContext};
        {false, Context} ->
            Response = mochijson2:encode({struct, [{disk_usage, []}]}),
            {Response, ReqData, Context}
    end.

-ifdef(TEST).
-endif.
