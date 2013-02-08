%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Helpers for multipart parsing.

-module(riak_cs_control_multipart).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([parse_multipart_response/1]).

%% @doc Strip leading carriage return and line feed so it's valid
%% multipart/mixed (this is the body seperator which erlcloud is not
%% properly handling).
-spec reformat_multipart(binary()) -> binary().
reformat_multipart(Content) ->
    list_to_binary(string:substr(binary_to_list(Content), 3)).

%% @doc Extract boundary from multipart header.
-spec extract_boundary(list()) -> list().
extract_boundary(ContentType) ->
    string:substr(ContentType, string:str(ContentType, "boundary=") +
                  length("boundary=")).

%% @doc Given a series of multipart documents, extract just body out and
%% parse based on content type.
-spec parse_bodies(list()) -> list().
parse_bodies(Parts) ->
    [mochijson2:decode(Body) ||
        {_Name, {_Params, _Headers}, Body} <- Parts].

%% @doc Given a series of parsed JSON documents, merge.
-spec merge_bodies(list()) -> list().
merge_bodies(Bodies) -> lists:flatten(Bodies).

%% @doc Parse multipart user response
-spec parse_multipart_response(list()) -> list().
parse_multipart_response(Response) ->
    Content = proplists:get_value(content, Response),
    ContentType = proplists:get_value(content_type, Response),
    ModifiedContent = reformat_multipart(Content),
    Boundary = extract_boundary(ContentType),
    Parts = webmachine_multipart:get_all_parts(ModifiedContent, Boundary),
    Bodies = parse_bodies(Parts),
    merge_bodies(Bodies).
