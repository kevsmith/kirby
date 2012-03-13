%% @author Kevin A. Smith <kevin@hypotheticalabs.com>
-module(kirby_store).

-behaviour(gen_server).

%% Opaque reference to eleveldb
-type md_store() :: any().
-type max_read_segs() :: {max_read_segs, non_neg_integer()}.
-type options() :: [max_read_segs()].

-record(state, {path="" :: string(),
                md :: undefined | md_store(),
                write_seg :: kirby_segment:seg() | undefined,
                read_segs=gb_trees:empty() :: gb_tree()}).


-export_type([options/0,
              max_read_segs/0]).

%% API
-export([open/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-spec open(file:filename(), options()) -> ok.
open(Path, Options) ->
    ok.
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
