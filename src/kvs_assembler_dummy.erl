%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_assembler_dummy).
-behaviour(kvs_assembler).

-export([ convert/3
        ]).
%%% API ========================================================================
convert(data, store_input, BinData) ->
  Data = binary_to_list(BinData),
  case string:tokens(Data, "|") of
    ["get", Key]        -> {get, Key};
    ["put", Key, Value] -> {put, Key, Value};
    ["del", Key]        -> {del, Key}
  end;
convert(store_output, data, Output) ->
  case Output of
    ok              -> <<"ok">>;
    {ok, Value}     -> <<"ok|", (ensure_type(Value))/binary>>;
    {error, Reason} -> list_to_binary("error|" ++ io_lib:format("~p", [Reason]))
  end.

%%% Internal Functions =========================================================
%% Note: this is really stupid, but for the dummy it is fine
ensure_type(Value) when is_binary(Value) -> Value;
ensure_type(Value) when is_list(Value)   -> list_to_binary(Value). 
