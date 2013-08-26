%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Dmitri Zagidulin <dzagidulin@basho.com>.
%% @copyright 2013 Basho Technologies, Inc.

%% @doc Helpers for Riak CS HTTP API calls.

-module(riak_cs_control_cs_api).
-author('Dmitri Zagidulin <dzagidulin@basho.com>').

-export([root_cs_url/0,
         admin_cs_url/0,
         disk_usage_url/0,
         cs_request/3,
         cs_request/4,
         cs_request/5,
         get_cs_disk_usage/0]).

root_cs_url() ->
    RiakCsProxyHost = riak_cs_control_configuration:cs_configuration(cs_proxy_host, "localhost"),
    RiakCsProxyPort = riak_cs_control_configuration:cs_configuration(cs_proxy_port, 8080),
    ["http://",RiakCsProxyHost,":",integer_to_list(RiakCsProxyPort),"/"].

admin_cs_url() ->
    root_cs_url() ++ [riak_cs_control_configuration:administration_bucket_name()] ++ ["/"].

disk_usage_url() -> 
    binary_to_list(iolist_to_binary([admin_cs_url(), "disk_usage"])).

%% @doc Return the result of an http call to /riak-cs/disk_usage http api
%% @spec get_cs_disk_usage() -> {ok, proplist()}|{error, term()}
get_cs_disk_usage() ->
    Url = disk_usage_url(),
    case cs_request(get, Url, ["200"]) of
        {ok, _Status, _Headers, Body} ->
            {struct, Response} = mochijson2:decode(Body),
            Stats = lists:flatten(Response),
            {ok, Stats};
        {error, Error} ->
            {error, Error}
    end.

%% @doc send an ibrowse request
cs_request(Method, Url, Expect) ->
    cs_request(Method, Url, Expect, [], []).
cs_request(Method, Url, Expect, Headers) ->
    cs_request(Method, Url, Expect, Headers, []).
cs_request(Method, Url, Expect, Headers, Body) ->
    Accept = {"Accept", "multipart/mixed, */*;q=0.9"},
    case ibrowse:send_req(Url, [Accept|Headers], Method, Body,
                          [{response_format, binary}]) of
        Resp={ok, Status, _, _} ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Error ->
            Error
    end.