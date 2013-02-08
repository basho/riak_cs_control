%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @author Christopher Meiklejohn <cmeiklejohn@basho.com>
%% @copyright 2012 Basho Technologies, Inc.

%% @doc Callbacks for the riak_cs_control application.

-module(riak_cs_control_app).
-author('Christopher Meiklejohn <cmeiklejohn@basho.com>').

-behaviour(application).
-export([start/2,stop/1]).

%% @doc application start callback for riak_cs_control.
-spec start(term(), term()) -> {'error',_} | {'ok',pid()} | {'ok',pid(),_}.
start(_Type, _StartArgs) ->
    riak_cs_control_sup:start_link().

%% @doc application stop callback for riak_cs_control.
-spec stop(term()) -> term().
stop(_State) ->
    ok.
