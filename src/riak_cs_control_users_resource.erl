%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Resource to manage users.

-module(riak_cs_control_users_resource).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         resource_exists/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {users=undefined}).

init([]) ->
    riak_cs_control_helpers:configure_s3_connection(),
    {ok, #context{users=undefined}}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case maybe_retrieve_users(Context) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Attempt to retrieve the user or return an exception if it doesn't
%% exist.
maybe_retrieve_users(Context) ->
    case Context#context.users of
        undefined ->
            try
                Response = erlcloud_s3:get_object(
                    riak_cs_control_helpers:administration_bucket_name(),
                    "users",
                    [{accept, "application/json"}]),
                ParsedResponse = parse_multipart_response(Response),
                {true, Context#context{users=ParsedResponse}}
            catch
                error:_ -> {false, Context}
            end;
        _Users ->
            {true, Context}
    end.

%% @doc Strip leading carriage return and line feed so it's valid
%% multipart/mixed.
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
    Response = case maybe_retrieve_users(Context) of
        {true, NewContext} ->
            Users = NewContext#context.users,
            mochijson2:encode({struct, [{users, Users}]});
        {false, Context} ->
            mochijson2:encode({struct, [{users, []}]})
    end,
    {Response, ReqData, Context}.

-ifdef(TEST).
-endif.
