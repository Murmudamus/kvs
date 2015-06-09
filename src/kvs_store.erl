%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_store).
-export([ close/2
        , create/4
        , del/3
        , get/3
        , open/2
        , put/4
        ]).
%%% Types ======================================================================
-type pmod()      :: any().
-type callback()  :: atom() | pmod().
-type key()       :: term().
-type value()     :: term().
-type args()      :: [{atom(), any()}].
-type ref()       :: any().
-export_type([ ref/0
             , callback/0
             , args/0
             ]).
%%% Behaviour ==================================================================
-callback close(ref()) -> ok.
-callback create(ref(), key(), value()) -> {ok, key()} | {error, already_exist}.
-callback del(ref(), key()) -> ok.
-callback get(ref(), key()) -> {ok, value()} | {error, not_found}.
-callback open(args()) -> {ok, ref()}.
-callback put(ref(), key(), value()) -> ok.
%%% API ========================================================================
-spec close(callback(), ref()) -> ok.
close(Callback, Reference) ->
  Callback:close(Reference).

-spec create(callback(), ref(), key(), value()) ->
                {ok, key()} | {error, already_exist}.
create(Callback, Reference, Key, Value) ->
  Callback:create(Reference, Key, Value).

-spec del(callback(), ref(), key()) -> ok.
del(Callback, Reference, Key) ->
  Callback:del(Reference, Key).

-spec get(callback(), ref(), key()) -> {ok, value()} | {error, not_found}.
get(Callback, Reference, Key) ->
  Callback:get(Reference, Key).

-spec open(callback(), args()) -> {ok, ref()}.
open(Callback, Args) ->
  Callback:open(Args).

-spec put(callback(), ref(), key(), value()) -> ok.
put(Callback, Reference, Key, Value) ->
  Callback:put(Reference, Key, Value).

%%% Internal Functions =========================================================
