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
         strip_root_from_attributes/1,
         iso8601/1,
         admin_key/0]).

strip_root_from_attributes(Attributes) ->
    {struct, [{<<"user">>, DecodedAttributes}]} = mochijson2:decode(Attributes),
    mochijson2:encode(DecodedAttributes).

configure_s3_connection() ->
    RiakCsHostname = s3_configuration(cs_hostname),
    RiakCsPort = s3_configuration(cs_port),
    RiakCsProtocol = s3_configuration(cs_protocol),
    RiakCsAccessKeyId = s3_configuration(cs_admin_key),
    RiakCsSecretAccessKey = s3_configuration(cs_admin_secret),
    RiakCsProxyHost = s3_configuration(cs_proxy_host, ""),
    RiakCsProxyPort = s3_configuration(cs_proxy_port, 0),
    erlcloud_s3:configure(RiakCsAccessKeyId,
                          RiakCsSecretAccessKey,
                          RiakCsHostname,
                          RiakCsPort,
                          RiakCsProtocol,
                          RiakCsProxyHost,
                          RiakCsProxyPort).

admin_key() ->
    s3_configuration(cs_admin_key).

administration_bucket_name() ->
    s3_configuration(cs_administration_bucket).

s3_configuration(Attribute) ->
    {ok, Value} = application:get_env(riak_cs_control, Attribute),
    Value.

s3_configuration(Attribute, Default) ->
    case application:get_env(riak_cs_control, Attribute) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

iso8601({{Y,M,D},{H,I,S}}) ->
    iolist_to_binary(
      io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                    [Y, M, D, H, I, S])).
