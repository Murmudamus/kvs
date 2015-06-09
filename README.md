# kvs
A key value store.
KVS starts an interface which starts its own key-value store. The store, assembler and listener backends can differ in every kvs instance. Each of these backends are based on a behaviour-based callback module system.
```
kvs_store -> kvs_store_ets
kvs_listener -> kvs_listener_tcp
kvs_assembler -> kvs_assembler_dummy
```

The listener is the interface to receive calls of which should be executed on the store.
The assembler translates the incoming payload to and from store-understandable values.
The store keeps the data. Persistance is based on the implementation of the store.

The listener, assembler and store will not necessarily have a 1-1-1 binding as a different listener can for instance use the same type of store and assembler.

# Application Requirements
KVS uses inets and thus requires it running.

# Example call
## Start a store
```
Args = [ {callback,      kvs_listener_tcp}
       , {callback_args, [ {port,       8888}
                         , {store,      kvs_store_ets}
                         , {store_args, [{name, tcp_store}]}
                         , {options,    [ {active, false}
                                        , binary
                                        , {packet, 0}
                                        ]}
                         , {assembler,  kvs_assembler_dummy}
                         , {mode,normal}
                         ]}
       ],
{ok, Pid} = kvs_sup:start_child(tcp_kvs, Args).
```
## Call store
Note that the payload depends on the assembler active for the particular kvs. In the example it is the dummy assembler.
```
{ok, Sock} = gen_tcp:connect("localhost", 8888, [binary, {packet, 0}, {active,false}]).
ok = gen_tcp:send(Sock, "get|1").
{ok, <<"error|not_found">>} = gen_tcp:recv(Sock, 0).
ok = gen_tcp:send(Sock, "put|1|me").
{ok, <<"ok">>} = gen_tcp:recv(Sock, 0).
ok = gen_tcp:send(Sock, "get|1").
{ok, <<"ok|me">>} = gen_tcp:recv(Sock, 0).
gen_tcp:close(Sock).
```