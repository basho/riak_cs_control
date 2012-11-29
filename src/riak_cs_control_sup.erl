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

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    RiakCsControlSession={riak_cs_control_session,
                          {riak_cs_control_session, start_link, []},
                          permanent,
                          5000,
                          worker,
                          [riak_cs_control_session]},

    case app_helper:get_env(riak_cs_control, enabled, false) of
        true ->
            Resources = [riak_cs_control_wm_asset,
                         riak_cs_control_wm_user,
                         riak_cs_control_wm_users],
            Routes = lists:append([Module:routes() || Module <- Resources]),
            [webmachine_route:add_route(R) || R <- Routes],

            {ok, { {one_for_one, 10, 10}, [RiakCsControlSession]}};
        _ ->
            {ok, { {one_for_one, 10, 10}, []}}
    end.
