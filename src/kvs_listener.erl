%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_listener).
-export([ start_link/3
        , stop/2
        ]).
%%% Types ======================================================================
-type pmod()     :: any().
-type callback() :: atom() | pmod().
-type ref()      :: pid().
-type id()       :: atom().
-type args()     :: [{atom(), any()}].
%%% Behaviour ==================================================================
-callback stop(ref()) -> ok.
-callback start_link(args(), id()) -> {ok, ref()} | {error, any()}.
%%% API ========================================================================
-spec stop(callback(), ref()) -> ok.
stop(Callback, Reference) ->
  Callback:stop(Reference).

-spec start_link(callback(), args(), id()) -> {ok, ref()} | {error, any()}.
start_link(Callback, Args, Id) ->
  Callback:start_link(Args, Id).

%%% Internal Functions =========================================================
