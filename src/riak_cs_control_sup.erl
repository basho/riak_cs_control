%% @author Christopher Meiklejohn <christopher.meiklejohn@gmail.com>
%% @copyright 2012 Christopher Meiklejohn.

%% @doc Supervisor for the riak_cs_control application.

-module(riak_cs_control_sup).
-author('Christopher Meiklejohn <christopher.meiklejohn@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, App} = application:get_application(?MODULE),
    {ok, Dispatch} = file:consult(filename:join([code:priv_dir(App),
                                                 "dispatch.conf"])),
    WebConfig = [
                 {name, http},
                 {ip, Ip},
                 {port, 8000},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    Web = {http,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    WebSSLConfig = [
                 {name, https},
                 {ip, Ip},
                 {port, 8443},
                 {ssl, true},
                 {ssl_opts, [{certfile, "/tmp/cert.pem"},
                    {keyfile, "/tmp/key.pem"}]},
                 {log_dir, "priv/log"},
                 {dispatch, Dispatch}],
    WebSSL = {https,
           {webmachine_mochiweb, start, [WebSSLConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    Processes = [Web, WebSSL],
    {ok, { {one_for_one, 10, 10}, Processes} }.
