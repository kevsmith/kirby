-define(WARN(Fmt, Args), error_logger:warning_msg(Fmt, Args)).
-define(ERR(Fmt, Args), error_logger:error_msg(Fmt, Args)).

-define(MAX_INT64, 18446744073709551615).
-define(MAX_INT16, 65535).

%% Enables file I/O mocking for tests
-ifndef(TEST).
-define(file, file).
-else.
-define(file, file_shim).
-endif.
