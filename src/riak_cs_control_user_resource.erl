%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Resource to manage one user.

-module(riak_cs_control_user_resource).
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

-record(context, {user=undefined}).

init([]) ->
    riak_cs_control_helpers:configure_s3_connection(),
    {ok, #context{user=undefined}}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

key_id(ReqData) ->
    wrq:path_info(key_id, ReqData).

resource_exists(ReqData, Context) ->
    case maybe_retrieve_user(Context, key_id(ReqData)) of
        {true, NewContext} ->
            {true, ReqData, NewContext};
        {false, Context} ->
            {false, ReqData, Context}
    end.

%% @doc Attempt to retrieve the user or return an exception if it doesn't
%% exist.
maybe_retrieve_user(Context, KeyId) ->
    case Context#context.user of
        undefined ->
            try
                Response = erlcloud_s3:get_object(
                    riak_cs_control_helpers:administration_bucket_name(),
                    "user/" ++ KeyId,
                    [{accept, "application/json"}]),
                RawUser = proplists:get_value(content, Response),
                User = mochijson2:decode(RawUser),
                {true, Context#context{user=User}}
            catch
                error:_ -> {false, Context}
            end;
        _User ->
            {true, Context}
    end.

%% @doc Return serialized user.
to_json(ReqData, Context) ->
    Response = case maybe_retrieve_user(Context, key_id(ReqData)) of
        {true, NewContext} ->
            User = NewContext#context.user,
            mochijson2:encode({struct, [{user, User}]});
        {false, Context} ->
            mochijson2:encode({struct, []})
    end,
    {Response, ReqData, Context}.

-ifdef(TEST).
-endif.
