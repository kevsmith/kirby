-module(kirby_format).

-include("kirby.hrl").
-include("kirby_common.hrl").

-type version1_entry() :: <<_:64,_:_*8>>.
-type version1_entry_md() :: <<_:152>>.
-type versioned_entry() :: version1_entry().
-type versioned_entry_md() :: version1_entry_md().
-type raw_entry() :: binary().
-type raw_md() :: binary().
-type int16() :: 0..?MAX_INT16.
-type int64() :: 0..?MAX_INT64.

-ifndef(TEST).
-export([make_seg_entry/1,
         parse_seg_entry/1,
         make_seg_md/3,
         parse_seg_md/1]).
-else.
-compile([export_all]).
-endif.

-spec make_seg_entry(binary()) -> {ok, versioned_entry()} | {error, too_big}.
make_seg_entry(Data) when is_binary(Data),
                          size(Data) > ?MAX_INT64 ->
    {error, too_big};
make_seg_entry(Data) ->
    make_versioned_seg(1, Data).

-spec parse_seg_entry(raw_entry()) -> {ok, non_neg_integer(), binary()} | {error, corrupted_entry | unknown_version}.
parse_seg_entry(Seg) when is_binary(Seg) ->
    parse_versioned_seg(Seg).

-spec make_seg_md(int64(), int64(), int16()) -> {ok, versioned_entry_md()} | {error, data_too_big |
                                                                              idx_too_big | offset_too_big}.

make_seg_md(Start, Size, SegIdx) when Start =< ?MAX_INT64,
                                      Size =< ?MAX_INT64,
                                      SegIdx =< ?MAX_INT16 ->
    make_versioned_seg_md(1, Start, Size, SegIdx);
make_seg_md(Start, _Size, _SegIdx) when Start > ?MAX_INT64 ->
    {error, offset_too_big};
make_seg_md(_Start, Size, _SegIdx) when Size > ?MAX_INT64 ->
    {error, data_too_big};
make_seg_md(_Start, _Size, SegIdx) when SegIdx > ?MAX_INT16 ->
    {error, idx_too_big}.

-spec parse_seg_md(raw_md()) -> {ok, char(), non_neg_integer(), non_neg_integer()} | {error, bad_md}.
parse_seg_md(<<SegIdx:16/integer, ?SEG_VERSION:8/integer, Start:64/integer, Size:64/integer>>) ->
    {ok, SegIdx, Start, Size};
parse_seg_md(Bin) when is_binary(Bin) ->
    {error, bad_md}.

%% Internal functions
make_versioned_seg(1, Data) when is_binary(Data) ->
    Checksum = erlang:adler32(Data),
    Size = size(Data),
    {ok, <<?SEG_MAGIC:24/integer, ?SEG_VERSION:8/integer, Checksum:32/integer,
           Size:64/integer, Data/binary>>}.

make_versioned_seg_md(1, Start, Size, SegIdx) ->
    {ok, <<SegIdx:16/integer, ?SEG_VERSION:8/integer, Start:64/integer, Size:64/integer>>}.

parse_versioned_seg(<<?SEG_MAGIC:24/integer, Vsn:8/integer, Rest/binary>>) when Vsn == ?SEG_VERSION ->
    case Rest of
        <<Checksum:32/integer, Size:64/integer, Data/binary>> ->
            case size(Data) == Size of
                true ->
                    {ok, Checksum, Data};
                false ->
                    {error, corrupted_entry}
            end;
        _ ->
            {error, corrupted_entry}
    end;
parse_versioned_seg(<<?SEG_MAGIC:24/integer, _:8/integer, _Rest/binary>>) ->
    {error, unknown_version};
parse_versioned_seg(Bin) when is_binary(Bin) ->
    {error, corrupted_entry}.

