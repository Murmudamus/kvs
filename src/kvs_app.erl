%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%% API ========================================================================
start(_Type, _StartArgs) ->
  kvs_sup:start_link().

stop(_State) ->
  ok.
