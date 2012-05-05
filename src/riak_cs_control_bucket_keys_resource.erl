%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Resource to serve keys in buckets.

-module(riak_cs_control_bucket_keys_resource).
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

bucket_name(ReqData) -> 
    dict:fetch(bucket, wrq:path_info(ReqData)).

list_objects(BucketName) -> 
    proplists:get_value(contents, erlcloud_s3:list_objects(BucketName)).

to_json(ReqData, Context) -> 
    riak_cs_control_helpers:configure_s3_connection(),
    BucketName = bucket_name(ReqData),
    Objects = list_objects(BucketName),
    Struct = mochijson_objects(Objects),
    {mochijson2:encode({struct, [{keys, Struct}]}), ReqData, Context}.

mochijson_objects(Objects) -> 
    [{struct, format_object(Object)} || Object <- Objects].

format_object(Object) -> 
    Key = list_to_binary(proplists:get_value(key, Object)),
    Size = proplists:get_value(size, Object),
    LastModified = riak_cs_control_helpers:iso8601(proplists:get_value(last_modified, Object)),
    [{key, Key}, {size, Size}, {last_modified, LastModified}].
