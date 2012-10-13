%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Resource to manage one user.

-module(riak_cs_control_user_resource).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    riak_cs_control_helpers:configure_s3_connection(),
    UserId = wrq:path_info(user_id, ReqData),
    User = erlcloud_s3:get_object("riak-cs", "/users/" ++ UserId),
    {mochijson2:encode({struct, [{user, {struct, User}}]}), ReqData, Context}.
