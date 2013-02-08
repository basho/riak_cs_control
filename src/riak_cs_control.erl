%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Riak CS control application.

-module(riak_cs_control).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @doc Starts the app for inclusion in a supervisor tree
-spec start_link() -> {ok,Pid::pid()}.
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    riak_cs_control_sup:start_link().

%% @doc Start the riak_cs_control server.
-spec start() -> ok.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(riak_cs_control).

%% @doc Stop the riak_cs_control server.
-spec stop() -> ok.
stop() ->
    Res = application:stop(riak_cs_control),
    _ = application:stop(webmachine),
    _ = application:stop(mochiweb),
    _ = application:stop(crypto),
    _ = application:stop(inets),
    Res.
