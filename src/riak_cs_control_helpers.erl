%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers.

-module(riak_cs_control_helpers).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([configure_s3_connection/0,
         administration_bucket_name/0,
         iso8601/1]).

configure_s3_connection() ->
    Hostname = s3_configuration(hostname),
    Port = s3_configuration(port),
    Protocol = s3_configuration(protocol),
    AccessKeyId = s3_configuration(access_key_id),
    SecretAccessKey = s3_configuration(secret_access_key),
    erlcloud_s3:configure(AccessKeyId,
                          SecretAccessKey,
                          Hostname,
                          Port,
                          Protocol).

administration_bucket_name() ->
    s3_configuration(administration_bucket).

s3_configuration(Attribute) ->
    {ok, Value} = application:get_env(riak_cs_control, Attribute),
    Value.

iso8601({{Y,M,D},{H,I,S}}) ->
    iolist_to_binary(
      io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                    [Y, M, D, H, I, S])).
