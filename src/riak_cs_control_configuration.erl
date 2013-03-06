%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers for configuration.

-module(riak_cs_control_configuration).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([configure_s3_connection/0,
         administration_bucket_name/0,
         cs_configuration/1]).

%% @doc Configure the erlcloud_s3 connection instance.
-spec configure_s3_connection() -> term().
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
                          RiakCsProxyPort,
                          []).

%% @doc Return the administration bucket name from the configuration.
-spec administration_bucket_name() -> term().
administration_bucket_name() ->
    cs_configuration(cs_administration_bucket).

%% @doc Return one configuration value from the environment.
-spec cs_configuration(term()) -> term().
cs_configuration(Attribute) ->
    {ok, Value} = application:get_env(riak_cs_control, Attribute),
    Value.

%% @doc Return one configuration value from the environment with default.
-spec cs_configuration(term(), term()) -> term().
cs_configuration(Attribute, Default) ->
    case application:get_env(riak_cs_control, Attribute) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.
