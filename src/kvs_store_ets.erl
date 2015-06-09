%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_store_ets).
-behaviour(kvs_store).

-export([ close/1
        , create/3
        , del/2
        , get/2
        , open/1
        , put/3
        ]).
%%% API ========================================================================
close(Reference) ->
  ets:delete(Reference),
  ok.

create(Reference, Key, Value) ->
  case ets:insert_new(Reference, {Key, encode(Value)}) of
    true  -> {ok, Key};
    false -> {error, already_exist}
  end.

del(Reference, Key) ->
  ets:delete(Reference, Key),
  ok.

get(Reference, Key) ->
  case ets:lookup(Reference, Key) of
    []             -> {error, not_found};
    [{Key, Value}] -> {ok, decode(Value)}
  end.

open(Args) ->
  {value, {name, Name}, Options} = lists:keytake(name, 1, Args),
  {ok, ets:new(Name, Options)}.

put(Reference, Key, Value) ->
  true = ets:insert(Reference, {Key, encode(Value)}),
  ok.

%%% Internal Functions =========================================================
encode(Value) -> term_to_binary(Value, [compressed]).

decode(Value) -> binary_to_term(Value).
