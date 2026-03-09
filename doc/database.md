Database
--------

To keep state, onomondo-eim uses an mnesia database. The database files are automatically created on the first startup.
The database contains three tables. `rest`, `work` and `euicc`.

### `rest` Table

In this table the rest resources are stored along with their status information. This is also the table where the REST
API user is maintaining the REST resources in. The column names are similar to the field names used on the REST API
interface.

* `resourceId`: the `resourceId` used to identify REST resources. The database automatically assigns a unique
  `resourceId` and returns it to the REST API user.
* `facility`: defines the `facility` to which the REST resource is associated.
* `eidValue`: contains the `EID` of the eUICC to which the `order` is associated.
* `order`: the specific `order` that the REST API has submitted
* `status`: overall status of the order (can be `new`, `work` or `done`)
* `timestamp`: unix-timestamp of when the last `status` change occurred
* `outcome`: the `outcome` of the execution of the `order`
* `debuginfo`: additional debug information, usually contains the last message seen on ESipa (for debug purpose only)

Example: displaying the contents of the `rest` table on the erlang shell
```
(onomondo_eim@127.0.0.1)6> mnesia_db:dump_rest().
[{rest,"398b29df-0239-4bbf-afda-ff2394472d7e",download,
       <<"89034011022310000000000006803372">>,
       {[{<<"download">>,
          {[{<<"activationCode">>,
             <<"1$smdpp.test.rsp.sysmocom.de$TS48V1-A-UNIQUE">>}]}}]},
       new,1719406618,[],none},
 {rest,"b307a73c-ef55-4454-b5b4-64a29d191084",psmo,
       <<"89034011022310000000000006803372">>,
       {[{<<"psmo">>,
          [{[{<<"enable">>,
              {[{<<"iccid">>,<<"989444999999990920F3">>},
                {<<"rollback">>,false}]}}]}]}]},
       new,1719406842,[],none},
 {rest,"fa97e7ac-bb02-49fb-ae8c-ba440b4fde0e",psmo,
       <<"89034011022310000000000006803372">>,
       {[{<<"psmo">>,
          [{[{<<"enable">>,
              {[{<<"iccid">>,<<"989444999999990930F1">>},
                {<<"rollback">>,false}]}}]}]}]},
       new,1719406847,[],none}]
```

### `work` Table

The `status` in the `rest` table is changed from `new` to `work` as soon as the related procedure begins to execute.
To maintain the state of the specific procedure the `work` table is used. This table is kept in RAM only, which means
that its contents are volatile. This is a performance optimization. The work state may be updated quite frequently
and in case the eIM has to handle a lot of IPAd clients simultaneously it is better not to synchronize
each state change to the disk. The state in the `work` table is also only valuable during the execution of the
procedure and a restart of the eIM while a procedure is running forcefully will terminate a procedure anyway.

The entries in the `work` table are always associated to a specific erlang process, which is in particular the
child process that the HTTP server creates when the connection is made. It is important that the HTTP client keeps
the connection open until the procedure has ended. This means that each request that is part of the current
procedure must be done within the same connection. Otherwise the HTTP server will generate a new child process
for each request and it will not be possible to match the request with an entry in the `work` table. However, in ESipa
most of the requests contain a `tranactionId`. In case the `pid` of the HTTP server child process can not be matched,
the eIM will automatically search for an entry that matches the `transactionId` from the the request. If found, the
`pid` in the work table is updated and the procedure may proceed. This mechanism is intended to recover from an
unintended interruption of a connection.

The `work` table maintains the following columns:

* `pid`: contains the erlang process id of the HTTP server child process
* `resourceId`: the `resourceId` of the related REST resource in the rest table.
* `transactionId`: the `transactionId` of the ESipa request
* `eidValue`: the EID of the related eUICC
* `order`: a copy of the `order` from the `rest` table
* `state`: a procedure specific state (not to be confused with the `status` column of the `rest` table)

Example: displaying the contents of the `work` table on the erlang shell
```
(onomondo_eim@127.0.0.1)4> mnesia_db:dump_work().
[{work,<0.869.0>,"f1c35e08-5f33-4ee5-9c28-521a27f5bed2",
       <<197,240,221,255,153,225,76,227,178,188,130,10,148,12,
         228,5>>,
       <<"89882119900000000000000000000005">>,
       {[{<<"download">>,
          {[{<<"activationCode">>,
             <<"1$smdpp.test.rsp.sysmocom.de$TS48V1-A-UNIQUE">>}]}}]},
       #{smdpAddress => <<"smdpp.test.rsp.sysmocom.de">>}}]
```

### `euicc` Table

The `euicc` table contains the eUICC master data. This is in particular the EID and other meta information that is
required for the eIM to operate. The data in this table is usually automatically collected and updated by the eIM.
However, there may be situations where the REST API user wants to update certain parameters of the `euicc` table.
This is why the REST API allows to modify the contents of the `euicc` table via `orders` from the `euicc` `facility`

The `euicc` table maintains the following columns:

* `eidValue`: the EID of the related eUICC
* `counterValue`: the counter value that is used when signing eUICC packages
* `consumerEuicc`: a flag to tell the eIM that the eUICC on the remote end is a consumer eUICC using an IoT eUICC
emulation mode.
* `associationToken`: an integer number to associate an eIM configuration on the eUICC with this eIM
* `signPubKey`: set the public key used to verify eUICC package responses (only relevant for IoT eUICCs)
* `signAlgo`: set the algorithm used to verify eUICC package responses (only relevant for IoT eUICCs)

Example: displaying the contents of the `euicc` table on the erlang shell
```
(onomondo_eim@127.0.0.1)2> mnesia_db:dump_euicc().
[{euicc,<<"89123456789027484800000000011628">>,1000,false,1,
        <<"04BD2C55B28B4E801CA0B14F1912345676C7FF49764C88A2934C695912345F647549F3F16060BD9C8D793D95"...>>,
        <<"prime256v1">>},
 {euicc,<<"89123456900000000000000000000005">>,12,true,1,
        <<>>,<<"prime256v1">>}]
(onomondo_eim@127.0.0.1)3>
```

#### eUICC Public Key

The eUICC public key information (`signPubKey` and `signAlgo`) can be set via the `euicc` facility of the REST API.
However, it is not necessary to do so. In case no `signPubKey` is set, onomondo-eim will automatically learn the
the public key information from the eUICC certificate that is exchanged during a profile download or an eUICC data
request.

#### Entry Creation

As already mentioned, the eIM will automatically collect the information needed to populate the `euicc` table. The
process begins with the first appearance of a new eID on the REST API. This means that the eIM will automatically
create an entry for a new eUICC only as a consequence of REAT API operations. ESipa requests from an unknown eID
will have no effect.

### Restart Behavior

In case the onomondo_eim instance restarts (either normally or due to a system crash). All orders that are currently
in progress (`status` = `work`) are terminated. Since the `work` table is kept in RAM only it will naturally lose its
contents. When onomondo-eim restarts it will go through the `rest` table to set the status of each rest resource that
still has its `status` set to `work` to `done`. To make the REST API user aware of the situation an appropriate
`procedureError` code is set ase well (`abortedOrder`).
