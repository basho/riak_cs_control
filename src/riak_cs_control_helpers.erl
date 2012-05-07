%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Helpers.

-module(riak_cs_control_helpers).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-export([configure_s3_connection/0,
         iso8601/1]).

configure_s3_connection() -> 
    {ok, Hostname} = application:get_env(riak_cs_control, hostname),
    {ok, AccessKeyId} = application:get_env(riak_cs_control, access_key_id),
    {ok, SecretAccessKey} = application:get_env(riak_cs_control, secret_access_key),
    erlcloud_s3:configure(AccessKeyId, SecretAccessKey, Hostname).

iso8601({{Y,M,D},{H,I,S}}) ->
    iolist_to_binary(
      io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                    [Y, M, D, H, I, S])).
