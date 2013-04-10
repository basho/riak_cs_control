%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Supervisor for the riak_cs_control application.

-module(riak_cs_control_sup).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc supervisor callback.
init([]) ->
    RiakCsControlSession={riak_cs_control_session,
                          {riak_cs_control_session, start_link, []},
                          permanent,
                          5000,
                          worker,
                          [riak_cs_control_session]},

    Ip = case os:getenv("WEBMACHINE_IP") of
        false -> "0.0.0.0";
        Any -> Any end,

    Port = riak_cs_control_configuration:cs_configuration(port),

    Resources = [riak_cs_control_wm_user,
                 riak_cs_control_wm_users,
                 riak_cs_control_wm_asset],

    Dispatch = lists:flatten([Module:routes() || Module <- Resources]),

    WebConfig = [
                 {name, http},
                 {ip, Ip},
                 {port, Port},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],

    Web = {http,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    Processes = [Web, RiakCsControlSession],

    lager:info("Application listening on port ~p", [Port]),

    {ok, {{one_for_one, 10, 10}, Processes}}.
