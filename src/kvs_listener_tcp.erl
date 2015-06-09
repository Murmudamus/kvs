%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
-module(kvs_listener_tcp).
-behaviour(kvs_listener).
-behaviour(gen_server).

-export([ stop/1
        , start_link/1
        , start_link/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {active    :: boolean() | once,
                mode      :: silent | normal,
                assembler :: kvs_assembler:callback(),
                lsocket, %% listen socket
                socket,
                store     :: kvs_store:callback(),
                store_ref :: kvs_store:ref()
               }).
%%% API ========================================================================
%% Args :: [ {port,       integer()}
%%         , {options,    [gen_tcp:listen_option()]}
%%         , {assembler,  kvs_assembler:callback()}
%%         , {mode,       silent | normal}
%%         , {store,      kvs_store:callback()}
%%         , {store_args, kvs_store:args()}
%%         ]
start_link(Args) -> start_link(Args, ?MODULE).

start_link(Args, Id) ->
  gen_server:start_link({local, Id}, ?MODULE, Args, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

%%% Gen Server Callbacks =======================================================
init(Args) ->
  Port      = proplists:get_value(port, Args),
  Options   = proplists:get_value(options, Args, [{active, true}, binary]),
  Store     = proplists:get_value(store, Args),
  StoreArgs = proplists:get_value(store_args, Args),
  case gen_tcp:listen(Port, Options) of
    {error, Reason}    -> {error, Reason};
    {ok, ListenSocket} ->
      accepting(),
      {ok, StoreRef} = kvs_store:open(Store, StoreArgs),
      {ok, #state{active    = proplists:get_value(active, Options),
                  mode      = proplists:get_value(mode, Args, normal),
                  assembler = proplists:get_value(assembler, Args),
                  lsocket   = ListenSocket,
                  store     = Store,
                  store_ref = StoreRef
                 }}
  end.

handle_call(stop, _From, State)    -> {stop, normal, State};
handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, Socket, Data}, #state{socket=Socket} = State) ->
  process(Data, State),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State)                          ->
  {ok, NewSocket} = gen_tcp:accept(State#state.lsocket),
  {noreply, setup_socket(State#state{socket=NewSocket})};
handle_info(stop, State)                                           ->
  {stop, normal, State};
handle_info(Msg, State)                                            ->
  error_logger:info_report([ {event, unknown_info}
                           , {msg,   Msg}
                           , {state, State}
                           ]),
  {noreply, State}.

terminate(_Reason, State) ->
  gen_tcp:close(State#state.socket),
  gen_tcp:close(State#state.lsocket),
  kvs_store:close(State#state.store, State#state.store_ref),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
%%% Internal Functions =========================================================
setup_socket(#state{active=false} = State) ->
  inet:setopts(State#state.socket, [{active, once}]),
  State;
setup_socket(State)                        -> State.

process(Data, State) ->
  #state{assembler=Assembler, store=Store, store_ref=StoreRef} = State,
  Input  = kvs_assembler:convert(Assembler, data, store_input, Data),
  Result = call_store(Input, Store, StoreRef),
  send_reply(Result, State),
  post_process(State).

%% TODO: log failed replies
send_reply(_Result, #state{mode=silent}) -> ok;
send_reply(Result, State)                ->
  #state{assembler=Assembler, socket=Socket} = State,
  Data = kvs_assembler:convert(Assembler, store_output, data, Result),
  gen_tcp:send(Socket, Data),
  ok.

post_process(#state{active=true})          -> ok;
post_process(#state{active=once})          -> accepting(), ok;
post_process(#state{active=false} = State) ->
  inet:setopts(State#state.socket, [{active, once}]),
  ok.

accepting() -> self() ! {tcp_closed, undefined}.

call_store({get, Key}, Store, StoreRef)        ->
  kvs_store:get(Store, StoreRef, Key);
call_store({put, Key, Value}, Store, StoreRef) ->
  kvs_store:put(Store, StoreRef, Key, Value);
call_store({del, Key}, Store, StoreRef)        ->
  kvs_store:del(Store, StoreRef, Key).
