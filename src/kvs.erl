%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs).

-export([ start_link/1
        , stop/2
        ]).

%%% API ========================================================================
%% -type pmod()          :: any().
%% -type callback()      :: atom() | pmod().
%% -type callback_args() :: [{atom(), any()}].
-type args() :: [{atom(), any()}].
%% args() :: [ {listener_id,   atom()}
%%           , {callback,      callback()}
%%           , {callback_args, callback_args()}]
-spec start_link(args()) -> {ok, pid()} | {error, any()}.
start_link(Args) ->
  ListenerId   = proplists:get_value(listener_id, Args),
  Callback     = proplists:get_value(callback, Args),
  CallbackArgs = proplists:get_value(callback_args, Args),
  kvs_listener:start_link(Callback, CallbackArgs, ListenerId).

stop(Callback, Pid) ->
  kvs_listener:stop(Callback, Pid).

%%% Internal Functions =========================================================
