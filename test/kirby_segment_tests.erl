-module(kirby_segment_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BLOB_PATH, "/tmp/kirby_tests").

reset_dir(Path) ->
    os:cmd("rm -rf " ++ Path),
    filelib:ensure_dir(filename:join([Path, "dummy"])).

noseg_setup(Error) ->
    reset_dir(?BLOB_PATH),
    mock_file_shim:init(Error).

seg_write_setup(Error) ->
    reset_dir(?BLOB_PATH),
    mock_file_shim:init(Error),
    {ok, Seg} = kirby_segment:open_for_write(?BLOB_PATH),
    Seg.

seg_read_setup(Error) ->
    reset_dir(?BLOB_PATH),
    mock_file_shim:init(Error),
    {ok, SegW} = kirby_segment:open_for_write(?BLOB_PATH),
    {ok, SegR} = kirby_segment:open_for_read(?BLOB_PATH, 1),
    {SegW, SegR}.

teardown(_, _) ->
    meck:unload().

open_for_write_failure_test_() ->
    {foreachx, fun noseg_setup/1, fun teardown/2,
     [{bad_blob_open,
       fun(_, _) ->
               [?_assertMatch({error, enoent}, kirby_segment:open_for_write(?BLOB_PATH))] end},
      {no_seg_lock,
       fun(_, _) ->
               [?_assertMatch({error, lock_failed}, kirby_segment:open_for_write(?BLOB_PATH))] end}]}.

writing_failure_test_() ->
    {foreachx, fun seg_write_setup/1, fun teardown/2,
      [{no_space_seg,
       fun(_, Seg) ->
               [?_assertMatch({error, enospc}, kirby_segment:write(Seg, <<"testing">>))] end},
       {bad_sync,
        fun(_, Seg) ->
                [?_assertMatch({error, fsync}, kirby_segment:write(Seg, <<"testing">>))] end},
       {bad_sync,
        fun(_, Seg) ->
                [?_assertMatch(ok, kirby_segment:close(Seg))] end},
       {bad_position,
        fun(_, Seg) ->
                [?_assertMatch({error, ebadf}, kirby_segment:write(Seg, <<"testing">>))] end},
       {bad_position_for_abort,
        fun(_, Seg) ->
                [?_assertMatch({error, enospc}, kirby_segment:write(Seg, <<"testing">>))] end},
       {bad_abort,
        fun(_, Seg) ->
                [?_assertMatch({error, enospc}, kirby_segment:write(Seg, <<"testing">>))] end}]}.

close_failure_test_() ->
    {foreachx, fun seg_write_setup/1, fun teardown/2,
     [{seg_close_fail,
       fun(_, Seg) ->
               [?_assertMatch({error, enomem}, kirby_segment:close(Seg))] end},
      {lock_close_fail,
       fun(_, Seg) ->
               [?_assertMatch(ok, kirby_segment:close(Seg))] end}]}.

successful_write_test_() ->
    {foreachx, fun seg_write_setup/1, fun teardown/2,
     [{success,
       fun(_, Seg) ->
               [?_assertMatch({ok, _S, _Sz}, kirby_segment:write(Seg, <<"testing 123">>))] end},
      {success,
       fun(_, Seg) ->
               fun() ->
                       {ok, S, _Sz} = kirby_segment:write(Seg, <<"test">>),
                       {ok, S1, _Sz1} = kirby_segment:write(Seg, <<"testing">>),
                       {ok, Seg1} = kirby_segment:open_for_write(?BLOB_PATH),
                       {ok, _S2, _Sz2} = kirby_segment:write(Seg1, <<"test1">>),
                       ?assertMatch(ok, kirby_segment:close(Seg)),
                       ?assertMatch(ok, kirby_segment:close(Seg1)),
                       ?assert(S1 > S) end end},
      {success,
       fun(_, Seg) ->
               fun() ->
                       ?assertMatch(ok, kirby_segment:close(Seg)),
                       {ok, Seg1} = kirby_segment:open_for_write(?BLOB_PATH),
                       {ok, _S, _Sz} = kirby_segment:write(Seg1, <<"test">>),
                       ?assertMatch(ok, kirby_segment:close(Seg1)) end end}]}.

open_for_read_failure_test_() ->
    {foreachx, fun noseg_setup/1, fun teardown/2,
     [{bad_blob_open,
       fun(_, _) ->
               [?_assertMatch({error, enoent}, kirby_segment:open_for_read(?BLOB_PATH, 1))] end}]}.

bad_read_test_() ->
    {foreachx, fun seg_read_setup/1, fun teardown/2,
     [{ebadf,
      fun(_, {SegW, SegR}) ->
              fun() ->
                      {ok, S, Sz} = kirby_segment:write(SegW, <<"test">>),
                      ?assertMatch({error, ebadf}, kirby_segment:read(SegR, S, Sz)) end end},
      {success,
       fun(_, {SegW, _SegR}) ->
               fun() ->
                       {ok, S, Sz} = kirby_segment:write(SegW, <<"test">>),
                       ?assertMatch({error, write_only}, kirby_segment:read(SegW, S, Sz)) end end}]}.

successful_read_test_() ->
    {foreachx, fun seg_read_setup/1, fun teardown/2,
     [{success,
       fun(_, {SegW, SegR}) ->
               fun() ->
                       Data = <<"testing 123">>,
                       {ok, S, Sz} = kirby_segment:write(SegW, Data),
                       ?assertMatch({ok, Data}, kirby_segment:read(SegR, S, Sz)) end end}]}.
