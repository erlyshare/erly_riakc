%%%-------------------------------------------------------------------
%%% @author erly
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2017 下午2:45
%%%-------------------------------------------------------------------
-module(erly_riakc).
-author("erly").
-include_lib("logger.hrl").
-include_lib("riakc/include/riakc.hrl").
%% API
-export([
	pool_name/0,
	get_worker/0,
	is_connected/0,
	ping/0,
	ping/1,
	sync_put/1,
	sync_put/2,
	sync_get/2,
	sync_get/3,
	update_type/3,
	delete/2,
	fetch_type/2,
	list_keys/1,
	list_buckets/1,
	create_new_map/1,
	mapred/2,
	search/2,
	get_index/4,
	get_index_range/5
]).

-type riakc_map_op() :: {{binary(), MapDataType :: atom()},
	fun((riakc_datatype:datatype()) -> riakc_datatype:datatype())}.

-define(CALL(__Fun__, __Args__), call_riak(__Fun__, __Args__)).

%% ========================================================================================
%%                                      外部 API
%% ========================================================================================
pool_name() -> riak_pool.

-spec get_worker() -> pid() | undefined.
get_worker() ->
	case catch cuesport:get_worker(pool_name()) of
		Pid  when is_pid(Pid) ->
			Pid;
		_ ->
			undefined
	end.

-spec is_connected() -> true | {false, [connection_failure()]}.
is_connected() ->
	?CALL(is_connected, []).

-spec ping() -> pong.
ping() ->
	?CALL(ping, []).

-spec ping(timeout()) -> pong.
ping(Timeout) ->
	?CALL(ping, [Timeout]).

-spec sync_put(riakc_obj()) ->
	ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
sync_put(Obj) ->
	sync_put(Obj, []).

-spec sync_put(riakc_obj(), timeout() | put_options()) ->
	ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
sync_put(Obj, OptsOrTimeout) ->
	?CALL(put, [Obj, OptsOrTimeout]).

-spec sync_get(bucket() | {binary(), bucket()}, key()) -> {ok, riakc_obj()} | {error, term()}.
sync_get(Bucket, Key) ->
	sync_get(Bucket, Key, []).

-spec sync_get(bucket() | {binary(), bucket()}, key(), get_options() | timeout()) ->
	{ok, riakc_obj()} | {error, term()}.
sync_get(Bucket, Key, OptsOrTimeout) ->
	?CALL(get, [Bucket, Key, OptsOrTimeout]).


-spec update_type({binary(), binary()}, binary(), riakc_datatype:update(term())) ->
	ok | {error, term()}.
update_type(Bucket, Key, Update) ->
	update_type(Bucket, Key, Update, []).

-spec update_type({binary(), binary()}, binary(),
	riakc_datatype:update(term()), [proplists:property()]) ->
	ok | {error, term()}.
update_type(Bucket, Key, Update, Options) ->
	?CALL(update_type, [Bucket, Key, Update, Options]).

-spec delete(bucket() | {binary(), bucket()}, key()) ->
	ok | {error, term()}.
delete(Bucket, Key) ->
	delete(Bucket, Key, []).

-spec delete(bucket() | {binary(), bucket()}, key(), delete_options() | timeout()) ->
	ok | {error, term()}.
delete(Bucket, Key, OptsOrTimeout) ->
	?CALL(delete, [Bucket, Key, OptsOrTimeout]).

-spec fetch_type({binary(), binary()}, binary()) ->
	{ok, riakc_datatype:datatype()} | {error, term()}.
fetch_type(Bucket, Key) ->
	fetch_type(Bucket, Key, []).

-spec fetch_type({binary(), binary()}, binary(), [proplists:property()]) ->
	{ok, riakc_datatype:datatype()} | {error, term()}.
fetch_type(Bucket, Key, Opts) ->
	?CALL(fetch_type, [Bucket, Key, Opts]).

-spec list_keys({binary(), binary()}) ->
	{ok, [binary()]} | {error, term()}.
list_keys(Bucket) ->
	?CALL(list_keys, [Bucket]).

-spec list_buckets(binary()) -> list().
list_buckets(Type) ->
	?CALL(list_buckets, [Type]).

-spec create_new_map([riakc_map_op()]) -> riakc_map:crdt_map().
create_new_map(Ops) ->
	update_map(riakc_map:new(), Ops).

-spec update_map(riakc_map:crdt_map(), [riakc_map_op()]) -> riakc_map:crdt_map().
update_map(Map, Ops) ->
	lists:foldl(fun update_map_op/2, Map, Ops).

-type mapred_bucket_type_idx_input() :: {index, riakc_obj:bucket(),
	binary()|secondary_index_id(),
	StartKey::key()|integer(),
	EndKey::key()|integer()}.

-spec mapred(mapred_inputs() |  mapred_bucket_type_idx_input(), [mapred_queryterm()]) ->
	{ok, mapred_result()} | {error, term()}.
mapred(KeyFileters, MapRed) ->
	?CALL(mapred, [KeyFileters, MapRed]).

search(Index, Query) ->
	search(Index, Query, []).

search(Index, Query, Opts) ->
	?CALL(search, [Index, Query, Opts]).

-spec get_index(Bucket :: riakc_obj:bucket(),
	Index :: binary() | secondary_index_id(),
	Key :: key() | integer(),
	Opts :: [term()]) ->
	{ok, index_results()} | {error, term()}.
get_index(BucketType, Index, Value, Opts) ->
	?CALL(get_index_eq, [BucketType, Index, Value, Opts]).

-spec get_index_range(Bucket :: riakc_obj:bucket(),
	Index :: binary() | secondary_index_id(),
	StartKey :: key() | integer() | list(),
	EndKey :: key() | integer() | list(),
	Opts :: [term()]) ->
	{ok, index_results()} | {error, term()}.
get_index_range(Bucket, Index, StartKey, EndKey, Opts) ->
	?CALL(get_index_range, [Bucket, Index, StartKey, EndKey, Opts]).



%% ========================================================================================
%%                                      内部 API
%% ========================================================================================

call_riak(F, ArgsIn) ->
	Worker = get_worker(),
	Args = [Worker | ArgsIn],
	apply(riakc_pb_socket, F, Args).

update_map_op({Field, Fun}, Map) ->
	riakc_map:update(Field, Fun, Map).
