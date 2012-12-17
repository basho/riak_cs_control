%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers for configuration and formatting of responses.

-module(riak_cs_control_formatting).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([strip_root_node/1,
         format_user/1,
         iso8601/1]).

%% @spec strip_root_node(list()) -> list()
%% @doc Remove root node from user structure.
strip_root_node(Attributes) ->
    {struct, [{<<"user">>, DecodedAttributes}]} = mochijson2:decode(Attributes),
    mochijson2:encode(DecodedAttributes).


%% @spec format_user({term(), list()}) -> {term(), list()}
%% @doc Format a user object, by adding a admin key derived from the
%% running environment.
format_user({struct, Attributes}) ->
    RiakCsAdminKey = riak_cs_control_configuration:cs_configuration(cs_admin_key),
    KeyId = proplists:get_value(key_id, Attributes),
    {struct, Attributes ++ {admin, KeyId =:= RiakCsAdminKey}}.


%% @spec iso8601({{term(), term(), term()},
%%                {term(), term(), term()}}) -> binary()
%% @doc Format a date to an ISO8601 friendly binary.
iso8601({{Y,M,D},{H,I,S}}) ->
    iolist_to_binary(
      io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                    [Y, M, D, H, I, S])).
