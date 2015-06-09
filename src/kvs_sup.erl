%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_sup).
-behaviour(supervisor).

%% API
-export([ start_child/2
        , start_child/3
        , start_link/0
        , stop_child/1
        ]).

%% Supervisor callbacks
-export([ init/1 ]).
%%% Defines ====================================================================
-define(MAX_RESTART,  1).
-define(MAX_INTERVAL, 3).
-define(API,          kvs).
%%% API ========================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ChildID, Args) ->
  start_child(ChildID, defaults(), Args).

start_child(ChildID, ChildArgs, Args) ->
  Child = child(ChildID, ChildArgs, Args),
  supervisor:start_child(?MODULE, Child).

stop_child(ChildID) ->
  ok = supervisor:terminate_child(?MODULE, ChildID),
  ok = supervisor:delete_child(?MODULE, ChildID).

%%% Internal Functions =========================================================
init([]) ->
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_INTERVAL}, []}}.

defaults() ->
  {ok, Restart}  = application:get_env(kvs, restart_type),
  {ok, Shutdown} = application:get_env(kvs, shutdown),
  {ok, Type}     = application:get_env(kvs, child_type),
  {Restart, Shutdown, Type}.

child(Id, {Restart, Shutdown, Type}, Args0) ->
  Args = [{listener_id, Id} | Args0],
  {Id, {?API, start_link, [Args]}, Restart, Shutdown, Type, [?API]}.
