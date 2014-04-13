%% General Debuggery which can be started by compiling with debug on.
-ifdef(debug).
-define(DEBUG(Msg),error_logger:info_report( Msg )).
-define(DEBUG(Msg,Fmt),error_logger:info_report( io_lib:format(Msg, Fmt) )).
-else.
-define(DEBUG(Msg),ok).
-define(DEBUG(Msg,Fmt),ok).
-endif.

%% Explicit Error Message formatting.
-define(ERROR(Msg),exit({Msg,erlang:get_stacktrace()})).
-define(ERROR(Msg,Fmt),exit({io_lib:format(Msg,Fmt),erlang:get_stacktrace()})).

