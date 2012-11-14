%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Riak CS control application.

-module(riak_cs_control).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    riak_cs_control_sup:start_link().

%% @spec start() -> ok
%% @doc Start the riak_cs_control server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(riak_cs_control).

%% @spec stop() -> ok
%% @doc Stop the riak_cs_control server.
stop() ->
    Res = application:stop(riak_cs_control),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
