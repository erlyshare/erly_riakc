-ifndef(LOG_H).
-define(LOG_H, true).

-define(DEBUG(Msg),                 lager:debug(Msg)).
-define(DEBUG(Format, Args),        lager:debug(Format, Args)).

-define(INFO(Msg),                  lager:info(Msg)).
-define(INFO(Format, Args),         lager:info(Format, Args)).

-define(NOTICE(Msg),                lager:notice(Msg)).
-define(NOTICE(Format, Args),       lager:notice(Format, Args)).

-define(WARNING(Msg),               lager:warning(Msg)).
-define(WARNING(Format, Args),      lager:warning(Format, Args)).

-define(ERROR(Msg),                 lager:error(Msg)).
-define(ERROR(Format, Args),        lager:error(Format, Args)).

-define(CRITICAL(Msg),              lager:critical(Msg)).
-define(CRITICAL(Format, Args),     lager:critical(Format, Args)).

-define(ALERT(Msg),                 lager:alert(Msg)).
-define(ALERT(Format, Args),        lager:alert(Format, Args)).

-define(EMERGENCY(Msg),             lager:emergency(Msg)).
-define(EMERGENCY(Format, Args),    lager:emergency(Format, Args)).


-define(DEBUG_RECORD(Format, Record),   lager:debug(Format, [lager:pr(Record, ?MODULE)])).
-define(DEBUG_RECORD_ARGS(Format, Args),   lager:debug(Format, [lager:pr(Record, ?MODULE) || Record <- Args])).

-define(INFO_RECORD(Format, Record),    lager:info(Format, [lager:pr(Record, ?MODULE)])).
-define(INFO_RECORD_ARGS(Format, Args),    lager:info(Format, [lager:pr(Record, ?MODULE) || Record <- Args])).

-define(WARNING_RECORD(Format, Record), lager:warning(Format, [lager:pr(Record, ?MODULE)])).
-define(WARNING_RECORD_ARGS(Format, Args), lager:warning(Format, [lager:pr(Record, ?MODULE) || Record <- Args])).

-define(ERROR_RECORD(Format, Record),   lager:error(Format, [lager:pr(Record, ?MODULE)])).
-define(ERROR_RECORD_ARGS(Format, Args),   lager:error(Format, [lager:pr(Record, ?MODULE) || Record <- Args])).

-define(CRITICAL_RECORD(Format, Record),lager:critical(Format, [lager:pr(Record, ?MODULE)])).
-define(CRITICAL_RECORD_ARGS(Format, Args),lager:critical(Format, [lager:pr(Record, ?MODULE) || Record <- Args])).

-define(STACK_TRACE(Format, Args),
    lager:error(Format ++ "~nStacktrace:~s", Args ++ [lager:pr_stacktrace(erlang:get_stacktrace())])).

-define(WARNING_STACK_TRACE(__F,__Args),
    fun()->
            catch fun()-> throw(error) end(),
        ?STACK_TRACE(__F,__Args)
    end()).



-endif.

