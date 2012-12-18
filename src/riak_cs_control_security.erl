%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers for security of requests.

-module(riak_cs_control_security).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([csrf_token/2,
         is_protected/2]).

%% @doc Store a CSRF protection token in a cookie.
csrf_token(ReqData, Context) ->
    case get_csrf_token(ReqData, Context) of
        undefined ->
            binary_to_list(base64url:encode(crypto:rand_bytes(256)));
        Token ->
            Token
    end.

%% @doc Get the CSRF token from the cookie.
get_csrf_token(ReqData, _Context) ->
    wrq:get_cookie_value("csrf_token", ReqData).

%% @doc Ensure this request contains a valid csrf protection token.
is_valid_csrf_token(ReqData, Context) ->
    HeaderToken = wrq:get_req_header("X-CSRF-Token", ReqData),
    CookieToken = get_csrf_token(ReqData, Context),
    HeaderToken /= undefined andalso HeaderToken == CookieToken.

%% @doc Is this a protected method?
is_protected_method(ReqData) ->
    Method = wrq:method(ReqData),
    Method == 'POST' orelse Method == 'PUT'.

%% @doc Is this a protected?
is_protected(ReqData, Context) ->
    (is_null_origin(ReqData) or
     not is_valid_csrf_token(ReqData, Context)) and
    is_protected_method(ReqData).

%% @doc Check if the Origin header is "null". This is useful to look for
%% attempts at CSRF, but is not a complete answer to the problem.
is_null_origin(ReqData) ->
    case wrq:get_req_header("Origin", ReqData) of
        "null" ->
            true;
        _ ->
            false
    end.
