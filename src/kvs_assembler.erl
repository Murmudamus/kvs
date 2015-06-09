%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_assembler).
-export([ convert/4
        ]).
%%% Types ======================================================================
-type pmod()         :: any().
-type callback()     :: atom() | pmod().
-type input_type()   :: data | store_output.
-type output_type()  :: data | store_input.
-type store_input()  :: {atom(), term()} | {atom(), term(), term()}.
-type store_output() :: ok | {ok, term()} | {error, any()}.
-type data()         :: any().
-type input()        :: data() | store_output().
-type output()       :: data() | store_input().
-export_type([ input_type/0
             , output_type/0
             , input/0
             , output/0
             , callback/0
             ]).
%%% Behaviour ==================================================================
-callback convert(input_type(), output_type(), input()) -> output().
%%% API ========================================================================
-spec convert(callback(), input_type(), output_type(), input()) -> output().
convert(Callback, InputType, OutputType, Input) ->
  Callback:convert(InputType, OutputType, Input).

%%% Internal Functions =========================================================
