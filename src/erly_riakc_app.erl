%%%-------------------------------------------------------------------
%% @doc erly_riakc public API
%% @end
%%%-------------------------------------------------------------------

-module(erly_riakc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include_lib("logger.hrl").

-export([
    start_wokers/0
]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erlang:process_flag(trap_exit, true),
    erlang:register(?MODULE, self()),
    {ok, SupPid} = erly_riakc_sup_sup:start_link(),
    start_wokers(),
    {ok, SupPid}.



%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_wokers() ->
    case enable() of
        true ->
            {Host, Port, PoolSize} = get_config(),
            RiakPBOpts = [auto_reconnect, keepalive],
            erly_riakc_sup:start(PoolSize, Host, Port, RiakPBOpts);
        false ->
            ?WARNING("not start riak.....")
    end.

enable() ->
    {ok, true} =:= application:get_env(erly_riakc,enable).

get_config() ->
    RiakOptions = application:get_all_env(erly_riakc),
    Host = proplists:get_value(host, RiakOptions),
    Port = proplists:get_value(port, RiakOptions),
    PoolSize = proplists:get_value(pool_size, RiakOptions, 20),
    {Host, Port, PoolSize}.