%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Callbacks for the riak_cs_control application.

-module(riak_cs_control_app).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for riak_cs_control.
start(_Type, _StartArgs) ->
    riak_cs_control_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for riak_cs_control.
stop(_State) ->
    ok.
