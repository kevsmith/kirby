-module(kirby_format).

-include("kirby.hrl").
-include("kirby_common.hrl").

-ifndef(TEST).
-export([make_seg_entry/1,
         parse_seg_entry/1,
         make_seg_md/3,
         parse_seg_md/1]).
-else.
-compile([export_all]).
-endif.


make_seg_entry(Data) when size(Data) > ?MAX_INT64 ->
    {error, too_big};
make_seg_entry(Data) ->
    Checksum = erlang:adler32(Data),
    Size = size(Data),
    <<?SEG_MAGIC:24/integer, ?SEG_VERSION:8/integer, Checksum:32/integer,
      Size:64/integer, Data/binary>>.

parse_seg_entry(Entry) ->
    <<Magic:24/integer, Version:8/integer, Rest/binary>> = Entry,
    if
        Magic == ?SEG_MAGIC ->
            parse_versioned_seg(Version, Rest);
        true ->
            {error, corrupted_entry}
    end.

make_seg_md(Start, _Size, _SegIdx) when Start > ?MAX_INT64 ->
    {error, offset_too_big};
make_seg_md(_Start, Size, _SegIdx) when Size > ?MAX_INT64 ->
    {error, data_too_big};
make_seg_md(_Start, _Size, SegIdx) when SegIdx > ?MAX_INT16 ->
    {error, idx_too_big};
make_seg_md(Start, Size, SegIdx) ->
    {ok, <<SegIdx:16/integer, Start:64/integer, Size:64/integer>>}.

parse_seg_md(Md) when size(Md) /= 18 ->
    {error, bad_md};
parse_seg_md(Md) ->
    <<SegIdx:16/integer, Start:64/integer, Size:64/integer>> = Md,
    {ok, SegIdx, Start, Size}.

%% Internal functions
parse_versioned_seg(1, Rest) when size(Rest) < 12 ->
    {error, corrupted_entry};
parse_versioned_seg(1, Rest) ->
    <<Checksum:32/integer, Size:64/integer, Data/binary>> = Rest,
    if
        size(Data) == Size ->
            {ok, Checksum, Data};
        true ->
            {error, corrupted_entry}
    end;
parse_versioned_seg(_Version, _Rest) ->
    {error, unknown_version}.
