-type kirby_md_err() :: {error, corrupted_entry | unknown_version}.
-type kirby_seg_lock()  :: {ok, string(), kirby_fd()}.
-type kirby_seg_error() :: {error, atom()}.


-type kirby_seg_open() :: {ok, #kirby_seg{}}.
