%%%-------------------------------------------------------------------
%% @doc erly_riakc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erly_riakc_sup).

-behaviour(supervisor).

%% API
-export([start/4]).
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start(PoolSize, Host, Port, RiakOpts) ->
    ChildSpec = {?MODULE, {?MODULE, start_link, [PoolSize, Host, Port, RiakOpts]},
        transient, infinity, supervisor, [?MODULE]},
    {ok, _} = supervisor:start_child(erly_riakc_sup_sup, ChildSpec).

start_link(PoolSize, Host, Port, RiakOpts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PoolSize, Host, Port, RiakOpts]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([PoolSize, Host, Port, RiakOpts]) ->
    {ok, {{one_for_one, 10, 10}, [child_spec(PoolSize, [Host, Port, RiakOpts])] }}.

%%====================================================================
%% Internal functions
%%====================================================================



child_spec(PoolSize, RiakOpts) ->
    ChildMods = [riakc_pb_socket],
    ChildMF = {riakc_pb_socket, start_link},
    RiakPoolName = erly_riakc:pool_name(),
    ChildArgs = {for_all, RiakOpts},
    PoolMFA = {cuesport, start_link, [RiakPoolName, PoolSize, ChildMods, ChildMF, ChildArgs]},
    {RiakPoolName, PoolMFA, transient, 2000, supervisor, ChildMods}.