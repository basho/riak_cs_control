%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers for formatting of responses.

-module(riak_cs_control_formatting).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([format_incoming_user/1,
         format_users/1,
         format_user/1,
         iso8601/1]).

%% @doc Format an incoming user for an S3 request.
-spec format_incoming_user(list()) -> list().
format_incoming_user(Attributes) ->
    {struct, [{<<"user">>, DecodedAttributes}]} = mochijson2:decode(Attributes),
    mochijson2:encode(DecodedAttributes).

%% @doc Format a user, by ensuring that all keys are atomized.
-spec format_user(term()) -> term().
format_user({struct, _} = Attributes) ->
    {struct, AttributeList} = atomize(Attributes),
    {struct, append_admin_status(AttributeList)};
format_user(Attributes) ->
    Attributes.

%% @doc Given a proplist of attributes, add admin status.
-spec append_admin_status(list()) -> list().
append_admin_status(Attributes) ->
    RiakCsAdminKey = riak_cs_control_configuration:cs_configuration(cs_admin_key),
    BinaryAdminKey = list_to_binary(RiakCsAdminKey),
    KeyId = proplists:get_value(key_id, Attributes),
    lists:append(Attributes, [{admin, KeyId =:= BinaryAdminKey}]).

%% @doc Format an entire list of users.
-spec format_users(list()) -> list().
format_users(Users) -> [format_user(User) || User <- Users].

%% @doc Format a date to an ISO8601 friendly binary.
-spec iso8601({{term(), term(), term()},
               {term(), term(), term()}}) -> binary().
iso8601({{Y,M,D},{H,I,S}}) ->
    iolist_to_binary(
      io_lib:format("~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0bZ",
                    [Y, M, D, H, I, S])).

%% @doc Given a struct/proplist that we've received via JSON,
%% recursively turn the keys into atoms from binaries.
atomize({struct,L}) ->
    {struct, [{binary_to_atom(I, utf8), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.
