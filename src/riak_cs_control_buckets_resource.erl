%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Resource to serve bucket listings.

-module(riak_cs_control_buckets_resource).
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

list_buckets() -> 
    [{buckets, Buckets}] = erlcloud_s3:list_buckets(),
    Buckets.

to_json(ReqData, Context) -> 
    riak_cs_control_helpers:configure_s3_connection(),
    Buckets = list_buckets(),
    Struct = mochijson_buckets(Buckets),
    {mochijson2:encode({struct, [{buckets, Struct}]}), ReqData, Context}.

mochijson_buckets(Buckets) -> 
    [{struct, format_bucket(Bucket)} || Bucket <- Buckets].

format_bucket(Bucket) -> 
    Name = list_to_binary(proplists:get_value(name, Bucket)),
    CreationDate = riak_cs_control_helpers:iso8601(proplists:get_value(creation_date, Bucket)),
    [{name, Name}, {creation_date, CreationDate}].
