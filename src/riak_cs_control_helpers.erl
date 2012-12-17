%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers for configuration and formatting of responses.

-module(riak_cs_control_helpers).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([configure_s3_connection/0,
         administration_bucket_name/0,
         strip_root_node/1,
         iso8601/1,
         cs_configuration/1,
         format_user/1]).

%% @spec strip_root_node(list()) -> list()
%% @doc Remove root node from user structure.
strip_root_node(Attributes) ->
    {struct, [{<<"user">>, DecodedAttributes}]} = mochijson2:decode(Attributes),
    mochijson2:encode(DecodedAttributes).

%% @spec configure_s3_connection() -> term()
%% @doc Configure the erlcloud_s3 connection instance.
configure_s3_connection() ->
    RiakCsHostname = cs_configuration(cs_hostname),
    RiakCsPort = cs_configuration(cs_port),
    RiakCsProtocol = cs_configuration(cs_protocol),
    RiakCsAdminKey = cs_configuration(cs_admin_key),
    RiakCsAdminSecret = cs_configuration(cs_admin_secret),
    RiakCsProxyHost = cs_configuration(cs_proxy_host, ""),
    RiakCsProxyPort = cs_configuration(cs_proxy_port, 0),
    erlcloud_s3:configure(RiakCsAdminKey,
                          RiakCsAdminSecret,
                          RiakCsHostname,
                          RiakCsPort,
                          RiakCsProtocol,
                          RiakCsProxyHost,
                          RiakCsProxyPort).

%% @spec format_user({term(), list()}) -> {term(), list()}
%% @doc Format a user object, by adding a admin key derived from the
%% running environment.
format_user({struct, Attributes}) ->
    RiakCsAdminKey = cs_configuration(cs_admin_key),
    KeyId = proplists:get_value(key_id, Attributes),
    {struct, Attributes ++ {admin, KeyId =:= RiakCsAdminKey}}.

%% @spec administration_bucket_name() -> term()
%% @doc Return the administration bucket name from the configuration.
administration_bucket_name() ->
    cs_configuration(cs_administration_bucket).

%% @spec cs_configuration(term()) -> term()
%% @doc Return one configuration value from the environment.
cs_configuration(Attribute) ->
    {ok, Value} = application:get_env(riak_cs_control, Attribute),
    Value.

%% @spec cs_configuration(term(), term()) -> term()
%% @doc Return one configuration value from the environment with default.
cs_configuration(Attribute, Default) ->
    case application:get_env(riak_cs_control, Attribute) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

%% @spec iso8601({{term(), term(), term()}, {term(), term(), term()}}) -> binary()
%% @doc Format a date to an ISO8601 friendly binary.
iso8601({{Y,M,D},{H,I,S}}) ->
    iolist_to_binary(
      io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                    [Y, M, D, H, I, S])).
