-module(mock_file_shim).

-include_lib("eunit/include/eunit.hrl").

-define(FILE_OPS, [open, write, position,
                   truncate, sync, datasync,
                   close, delete]).

-define(POSIX_ERRORS, [eacces, eagain, ebadf, ebusy, edquot, eexist, efault,
                      efbig, eintr, einval, eio, eisdir, eloop, emfile, emlink,
                      enametoolong, enfile, enodev, enoent, enomem, enospc, enotblk,
                      enotdir, enotsup, enxio, eperm, epipe, erofs, espipe, esrch,
                      estale, exdev]).

-export([init/1]).

init(Error) ->
    meck:new(file_shim),
    [meck:expect(file_shim, Op, mock_fun(Op, Error)) || Op <- ?FILE_OPS].

mock_fun(open, bad_blob_open) ->
    fun(Path, Opts) -> case path_type(Path) of
                           seg ->
                               {error, enoent};
                           lock ->
                               open_path(Path, Opts)
                       end end;
mock_fun(write, no_space_all) ->
    fun(_Path, _Opts) -> {error, enospc} end;
mock_fun(write, no_space_seg) ->
    fun({seg, _Fd}, _Data) -> {error, enospc};
       ({lock, Fd}, Data) -> file:write(Fd, Data) end;
mock_fun(write, lock_err) ->
    fun({seg, Fd}, Data) -> file:write(Fd, Data);
       ({lock, _Fd}, _Data) -> {error, enospc} end;
mock_fun(write, bad_abort) ->
    fun({seg, _Fd}, _Data) -> {error, enospc};
       ({lock, Fd}, Data) -> file:write(Fd, Data) end;
mock_fun(write, bad_position_for_abort) ->
    mock_fun(write, bad_abort);
mock_fun(truncate, bad_abort) ->
    fun({_, _Fd}) -> {error, eio} end;
mock_fun(sync, bad_sync) ->
    fun(_Fd) -> {error, enospc} end;
mock_fun(datasync, bad_sync) ->
    fun(_Fd) -> {error, enospc} end;
mock_fun(position, bad_position) ->
    fun(_Fd, _Pos) -> {error, ebadf} end;
mock_fun(position, bad_position_for_abort) ->
    fun({seg, Fd}, Offset) -> case get_or_incr(position, 2) of
                                  ok ->
                                      file:position(Fd, Offset);
                                  stop ->
                                      {error, eio}
                              end;
       ({_, Fd}, Offset) -> file:position(Fd, Offset) end;
mock_fun(close, seg_close_fail) ->
    fun({seg, _Fd}) -> {error, enomem};
       ({lock, Fd}) -> file:close(Fd) end;
mock_fun(close, lock_close_fail) ->
    fun({seg, Fd}) -> file:close(Fd);
       ({lock, _Fd}) -> {error, enomem} end;
mock_fun(open, no_seg_lock) ->
    fun(Path, Opts) -> case path_type(Path) of
                           seg ->
                               open_path(Path, Opts);
                           lock ->
                               {error, eexists}
                       end end;
mock_fun(sync, _) ->
    fun({_, Fd}) -> file:sync(Fd) end;
mock_fun(datasync, _) ->
    fun({_, Fd}) -> file:datasync(Fd) end;
mock_fun(close, _) ->
    fun({_, Fd}) -> file:close(Fd) end;
mock_fun(write, _) ->
    fun({_, Fd}, Data) -> file:write(Fd, Data) end;
mock_fun(position, _) ->
    fun({_, Fd}, Pos) -> file:position(Fd, Pos) end;
mock_fun(truncate, _) ->
    fun({_, Fd}) -> file:truncate(Fd) end;
mock_fun(open, _) ->
    fun(Path, Opts) -> open_path(Path, Opts) end;
mock_fun(delete, _) ->
    fun(Path) -> file:delete(Path) end.

open_path(Path, Opts) ->
    case file:open(Path, Opts) of
        {ok, Fd} ->
            {ok, {path_type(Path), Fd}};
        Error ->
            Error
    end.

get_or_incr(Counter, Max0) ->
    Max = Max0 - 1,
    case erlang:get(Counter) of
        undefined ->
            erlang:put(Counter, 1),
            ok;
        Max ->
            stop;
        V ->
            erlang:put(Counter, V + 1),
            ok
    end.

path_type(Path) ->
    case string:str(Path, "lock-") of
        0 ->
            case string:str(Path, "segment-") of
                0 ->
                    {error, unknown_path};
                _ ->
                    seg
            end;
        _ ->
            lock
    end.
