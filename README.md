# Onomondo eIM
onomondo-eim is an erlang based eIM (Remote provisioning and management of the eUICC in IoT Devices, see also SGP.31)
implementation. Besides an ES9+ (SMDP+) and an ESipa (IPAd) interface it also features a REST API that allows an API
user to perform management tasks on the eUICC (profile download, execution of PSMOs, eCOs and eUICC data requests).

Interfaces
----------

### ESipa

The ESipa interface of onomondo-eim is implemented as an HTTP server interface (see also GSMA SGP.32, section 6.1) that
uses ASN.1 function bindings (see also GSMA SGP.32, section 6.3. The HTTP client (IPAd) on the remote end is expected to
establish the connection with the ESipa.GetEimPackage request and keep it open at least until the related procedure
ends with the ESipa.HandleNotifications request (see also GSMA SGP.32, section 3.1.1.1, figure 5)

### ES9+

The ES9+ interface of onomondo-eim is implemented as a HTTP client interface (see also GSMA SGP.22, section 5.6). that
uses the JSON function bindings (see also GSMA SGP.22, section 6.5).

### REST API

The REST API is a custom interface that is used by the eIM operator to manage a fleet of eUICCs. It is implemented
as a HTTP server and uses JSON formatted requests and responses. (see also section `REST API`)


Getting Started
---------------

### Installing run time dependencies

* Dependencies required to compile and run the onomondo_eim erlang application:
```
  apt-get install erlang
  apt-get install curl
  apt-get install build-essential
```

* Dependencies required to compile and run the restop.py tool
```
  apt-get install python3-full
  pip3 install erlang-py
  pip3 install requests
```

### Building and Running

The build process is started using `make`. The overall process may take a while since erlang.mk will also take care of
downloading and compiling further erlang related dependencies. The build process can also be started with `make run`.
This will start the application immediately after finishing the compilation.

A typical output after the first startup
```
Erlang/OTP 25 [erts-13.1.5] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [jit:ns]

=NOTICE REPORT==== 18-Jul-2024::11:54:10.513176 ===
eIM! version:1.0.0

=INFO REPORT==== 18-Jul-2024::11:54:10.513746 ===
    application: mnesia
    exited: stopped
    type: permanent

=NOTICE REPORT==== 18-Jul-2024::11:54:10.529194 ===
    mnesia database schema created

=NOTICE REPORT==== 18-Jul-2024::11:54:10.531832 ===
    mnesia started

=NOTICE REPORT==== 18-Jul-2024::11:54:10.533426 ===
    rest table created

=NOTICE REPORT==== 18-Jul-2024::11:54:10.534703 ===
    work table created

=NOTICE REPORT==== 18-Jul-2024::11:54:10.536872 ===
    euicc table created

=NOTICE REPORT==== 18-Jul-2024::11:54:10.537310 ===
Starting ESipa HTTPs server at {127,0,0,1}:8000...
certificate: "../../config/sample_ssl_cert.crt"
key: "../../config/sample_ssl_cert.key"

=NOTICE REPORT==== 18-Jul-2024::11:54:10.538272 ===
Starting REST HTTP server at {127,0,0,1}:8080...

Eshell V13.1.5  (abort with ^G)
(onomondo_eim@127.0.0.1)1>
```

Once the application is running it will display a prompt. To exit the application type `CTRL+G` and then `q`.

(To run onomondo-eim under the control of `systemd`, the example service file /contrib/onomondo-eim.service may be
used)

To verify that the ESipa interface is reachable it can be probed using a web-browser. In the example above, the ESipa
interface runs on 127.0.0.1, port 8000 and uses SSL. It can be probed by typing "https://127.0.0.1:8000/" into the
address line of a web-browser. The result should be the string "eIM working!". The REST interface can be probed with
the same method (see section `eIM Information`).

Configuration
-------------

The user relevant configuration files can be found in the `./config` directory. In the default configuration
onomondo-eim will start with all services bound to local host. The most relevant configuration file is
`sys.config`, which sets the application parameters. The file `vm.args` sets erlang VM parameters. In most
cases, those parameters do not have to be modified.

### sys.config

* `esipa_ip`: Configure on which network interface where the ESipa interface should listen.
* `esipa_port`: Configure the port number where the ESipa interface should listen.
* `esipa_ssl_disable`: Set this to true to use HTTP instead of HTTPs. This is a debug feature intended for lab setups to
simplify the creation of protocol traces.
* `esipa_ssl_cert`: Configure the location of the SSL certificate.
* `esipa_ssl_key`: Configure the location of the SSL private key.
* `rest_ip`: Configure on which network interface where the REST API interface should listen.
* `rest_port`: Configure the port number where the REST API interface should listen.
* `eim_id`: Configure the `eimId` of the eIM instance.
* `es9p_ssl_disable`: Set this to true to use HTTP instead of HTTPs. This is a debug feature intended for lab setups to
simplify the creation of protocol traces.
* `eim_cert`: Configure the location of the certificate that is used for verification of eUICC packages.
* `eim_key`: Configure the location of the private key that is used for signing eUICC packages.
* `counter_value`: Set the start value of the replay protection counter (eUICC packages).
* `consumer_euicc`: Set to true in case the remote ends use a consumer eUICC together with an IoT eUICC emulation mode.
* `rest_timeout_stuck`: Configure timeout until an order/procedure (e.g. profile download) must finish.
* `rest_timeout_noshow`: Configure timeout until an order/procedure must start.
* `rest_timeout_expired`: Configure timeout until the REST API user must lookup/delete the order via the REST API
* `root_ci_certs`: Configure root certificates of the eUICC CI (to verify EUM and eUICC certificate)

#### Timeout Behavior

The three REST API related timeouts (`rest_timeout_`) ensure that the underlying REST database of the REST API won't
overflow over time in case REST API users fail to monitor their orders and most importantly, delete their orders when
done.

* The timeout `rest_timeout_stuck` guards against stuck orders. Orders may get stuck due to communication errors between
  SMDP+ or IPAd. When a procedure is stuck for too long it gets marked as done and an appropriate `procedureError` code
  is communicated to the REST API user. Since a procedure usually won't take more than a few minutes (usually below one
  minute) 300 sec. would be a good compromise here.

* The timeout `rest_timeout_noshow` guards against IPAd/eUICCs that fail to show up. When an order is placed the IPAd
  is expected to poll the eIM within a reasonable amount of time. When the IPAd fails to poll for some reason, the
  order gets marked as done and an appropriate 'procedureError' code is communicated to the REST API user. Usually the
  polling is triggered through some side channel or the polling happens periodically. Depending on the situation the
  timeout may be set to several hours or even days. In any case it should not be lower than `rest_timeout_stuck` for
  obvious reasons. A recommended timeout value would be 1800 (30 min).

* The timeout `rest_timeout_expired` guards against careless REST API users. As mentioned already, the REST API user is
  expected to carefully monitor his orders and delete them on his own responsibility. However, it may be that a REST
  API user fails to monitor an order he created. To prevent the database from gradually overflowing, such orders will
  be expired. This means they are deleted silently. Depending on the situation and the quality of the tooling that
  operates the REST API, this timeout can be set generously (hours, days or even weeks). In any case, the timeout should
  not be set lower than `rest_timeout_noshow` for obvious reasons. A recommended timeout value would be 86400 (24h)

#### Consumer eUICCs

Consumer eUICCs are normally not used in an eIM/IPAd infrastructure. They are intended to be used in consumer devices
where the user controls the eUICC directly using an LPA software. However, it is still possible to use a consumer
eUICC like an IoT eUICC when the remote IPAd supports an IoT eUICC emulation mode. Unfortunately the emulation will
lack some features, this is in particular the cryptographic signing of the PSMOs / eCOs exchanged between the eIM and
the IoT eUICC emulation. To get around this, the eIM must know which eUICC is a real IoT eUICC and which eUICC is
actually a consumer eUICC hiding behind an IoT eUICC emulation.

Onomondo-eim supports the simultaneous usage of IoT eUICCs and consumer eUICCs. The type of the eUICC must be set for
each eUICC via the REST API once, if it differs from the default set with the configuration option `consumer_euicc`.

For larger installations, where both flavours of eUICCs are used, it is recommended to run two dedicated eIM
instances, one for each eUICC flavor.

### vm.args

* See also: https://www.erlang.org/doc/man/erl.html
* mnesia dir: configure the location of the mnesia database. The default setting will store the database in
  `./_rel/onomondo_eim_release/db`.
* kernel logger_level: configure the log level. The default setting will be `notice`. For lab testing, the log level
  may be set to `info` for moderate verbosity and `debug` for increased verbosity (includes all message contents).

REST API
--------

The REST API offered by onomondo-eim is a powerful interface to manage a fleet of eUICCs. The REST API lets the user
trigger profile downloads and offers full access to all PSMOs and eCOs that are specified in GSMA SGP.32.

### Facilities

The REST API is divided up into so called "facilities". The `facility` identifier is the first path element of the HTTP
URL. There are four facilities defined:

* `download`: management of profile downloads
* `psmo`: Profile State Management Operations (PSMO)
* `eco`: Eim Configuration Operations (eCO)
* `edr`: eUICC data request (see also GSMA SGP.32, section 2.11.1.2)
* `euicc`: eim-local eUICC configuration Operations

The purpose of the facilities is to provide separation at URL level. This allows for easier filtering to restrict
access for specific REST API users.

### Operations

The REST API defines four different basic operations. The name of the operation is the second path element of the HTTP URL:

* `create`: create a rest resource, returns a `resourceId`
* `lookup`: lookup a rest resource by its `resourceId`, returns JSON
* `delete`: delete a rest resource by its `resourceId`, returns JSON
* `list`: list all `resourceId`s available for the current facility, returns JSON.

### ResourceId

When a REST resource is created, a so called `resourceId` is returned by the REST API. The `resourceId` uniquely
identifies a REST resource and has to be memorized by the REST API user to perform further operations.

Example: URL with selected facility `download` and operation `lookup` on the `resourceId`
"8a901bd9-f203-4eae-bcba-12dee32f4444"
```
http://127.0.0.1:8080/download/lookup/8a901bd9-f203-4eae-bcba-12dee32f4444
```

### Interface

The REST API receives JSON formatted data via HTTP POST requests and returns a JSON formatted response. The following
chapter describes how the REST interface works in general.

#### Creating Orders

Each request contains a so called `resource`. The `resource` is a JSON formatted request that contains the `eidValue` and
a so called `order`. The `eidValue` identifies the eUICC and the `order` contains specific parameters that the eIM needs
to fulfill a specific task (e.g. trigger a profile download).

Example: Rest `resource` that orders to trigger a profile download
```
{ "eidValue" : "89882119900000000000000000000005", "order" : {"activationCode" : "1$rsp.example.com$EXAMPLE"}}'
```

#### Monitoring Orders

The REST API user is expected to poll the rest resource from time to time to check on its `status`. The polling is done
using the lookup operation:

Example: URL to lookup a specific REST `resource`
```
http://127.0.0.1:8080/download/lookup/8a901bd9-f203-4eae-bcba-12dee32f4444
```

The result will contain a `status` field along with some other information that allows the REST API user to monitor the
progress and the `outcome` of the current `order`.

The following fields are defined:

* `status`: contains the processing status of an `order`. This field does not say anything about success or failure of
an `order`. It just tells the processing `status`. When an `order` is new, the status will be `new`. An `order` that
is currently in progress will report `work` as `status`. When the `order` is finished, the reported `status` will be
`done`. In case of an REST resource error (non existing `resourceId`) the status will be `absent`. A REST resource
that has just been deleted will report `deleted` to confirm its deletion. Contrary to all other fields, the `status`
field is a mandatory field that is always present.
* `timestamp`: contains the `timestamp` of the last update. The `timestamp` may be used by the REST API user to
determine how long `orders` take.
* `resource`: The `resource` is a copy of the JSON object that was submitted when the REST resource was created. It
contains the `eidValue` and the `order`. It is included in the data so that the REST API user does not have to
memorize it.
* `outcome`: The `outcome` depends on the `facility` and on the specific `order`. Its purpose is to inform the REST
API user of the `order` results. The `outcome` is modeled as a list since an outcome may contain more then one result.
This is in particular true for eUICC packages with more than one PSMO or eCO. The `outcome` is also used to convey
error codes back to the REST API user in case there were problems during the execution of the `order`.
* `debuginfo`: The `debuginfo` contains the last ESipa message that was received from the IPAd in erlang ETF format.
During normal operation the field has no relevance. Its only purpose is to provide debug information to a software
engineer. It should also be noted that the structure of the contents of this field may changed without further
notice.

Example: A typical `lookup` response after a successful profile install:
```
{"status": "done", "timestamp": "1718881629", "resource": {"eidValue": "89086030202200000022000027485428", "order": {"download": {"activationCode": "1$rsp.example.com$EXAMPLE"}}}, "outcome": [{"profileInstallationResult": {"finalResult": "successResult", "iccid": "984474680000730771F0"}}], "debuginfo": "83680264001370656E64696E674E6F74696669636174696F6E680264001970..."}
```

Example: A typical `lookup` response when an invalid `resourceId` is used:
```
{"status": "absent"}
```

#### Deleting REST resources

The REST API user is responsible to keep the `rest` table clean. When an `order` is done or has to be revoked for some
reason, the REST API user will use the `delete` operation to remove the related REST resource from the `rest` table.

Example: URL to `delete` a specific REST `resource`
```
http://127.0.0.1:8080/download/delete/8a901bd9-f203-4eae-bcba-12dee32f4444
```

The eIM will confirm the deletion of the REST `resource` by responding with a status `deleted`.

Example: A typical delete response
```
{"status":"deleted"}
```

In case the API user forgets to delete the REST resource for some reason, the REST resource it will not stay in the
database indefinitely. After some generous timeout, the eIM will automatically delete the REST resource from the REST
table. However, the intention behind this mechanism is to guard against database memory leaks and is not intended to be
used as the general mechanism to get rid of REST resources that are no longer used.

#### Listing REST resources

The REST API user has to keep track of his REST resources. However, there may be circumstances where the REST API user
must recover his state. In this case it is possible to list all `resourceId`s of all REST resources that are currently
in the `rest` table.

Example: URL to list all REST `resource`s available in the `download` `facility`
```
http://127.0.0.1:8080/download/list
```

The eIM will respond with a `resourceIdList`. The REST API user may then use this list to lookup the state of each
REST resource. Since the list only contains the `resourceId`s of the current `facility`, the REST API user must repeat
the process for all `facilities`.

Example: A typical `list` response with three `resourceID`s
```
{"resourceIdList": ["2100f52e-83e1-4fed-9d30-f309daf3391a","35074412-654b-4d7b-aec0-e159837b998f","9d6b14df-1875-4827-8236-916383972a19"]}
```

#### eIM Information

The REST API also features an info page which can be used by the REST API user to gain some basic information of the eIM
that it is working with. The info page mainly reflects the eIM configuration applied in sys.config (see also section
"Configuration").

When an eIM is added to the eimConfiguration on the eUICC (either via ES10b:AddInitialEim or via addEim eCO) the data
is required to be passed in its ASN.1 encoded form. To simplify the process onomondo-eim also presents the eIM
configuration in two different ASN.1 encoded formats:

* `addInitialEimRequest`: This data format can be passed directly to an eUICC via an ES10b APDU. It will create the eIM
configuration on the eUICC so that the eUICC is associated with the current onomondo_eim instance. The eUICC will
create an `associationToken` in response, which should be passed back to the eIM via the REST API on the `euicc`
facility. However, on a virgin eUICC the `associationToken` will be 1, which is also the default onomondo-eim will use.
(see also GSMA SGP.32, section 3.5.2.1)

* `eimConfigurationData`: This data format has the same contents as `addInitialEimRequest`, but withut an envelope.
This format is suitable to be used for adding the eIM to the eUICC remotely via an eCO. (see also GSMA SGP.32, section
3.5.1.1)

Example: URL that returns the eIM information
```
http://127.0.0.1:8080/
```

#### JSON Schema

This description above describes only the basic concept of the REST API. To give a system integrator a detailed overview
on how the requests/responses should look like three JSON schema files are shipped with onomondo-eim:

* contrib/rest_api_resource_schema.json: This schema describes a REST resource. This is the format that the requests
issued to the REST API should have.

* contrib/rest_api_response_schema.json: This schema that describes the JSON formatted response that is received from the
REST API when the `lookup` operation is performed.

* contrib/rest_api_info_schema.json: This schema that describes the JSON formatted info page that is received when the
REST API is called without any parameters.

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

Practical examples
------------------

The REST API is complex interface that is difficult to operate out of the box without any prior familiarization. To
give a system integrator a good starting point, the contrib directory contains "tryme-scripts" that serve as examples
and an easy way try out the REST API.

### Scripts

There is a tryme script to `create` downloads (tryme_download.sh) and one tryme script per PSMO/eCO. The scripts are
called with a one letter parameter that refers to a profile that is pre-configured in tryme.cfg (see below). To get
an overview which profiles are available/preconfigured, a "tryme_*.sh" script may be called without parameters.

It should be noted that the tryme_*.sh scripts are really just simple examples that were created to simplify testing
during development.

#### restop.py

The python-script "restop.py" is called by the tryme "tryme_*.sh" scripts. This script can be used as a stand-alone
tool to operate the REST API. The intended usecase of "restop.py" is to offer an easy access to the REST API for
testing and development. It should be noted that this tool can not replace a proper eUICC management backend. A REST API
user must keep track of the REST resources he created, monitor them, check for errors, delete REST resources, resubmit
REST resources in case an `order` has failed, etc.

#### tryme.cfg

This is just a simple shellscript that sets some initial variables. Some sample values are already present. It is
recommended to edit tryme.cfg and to replace the sample values with some useful values. This is in particular the
$EID variable at the top of the file.

The file also defines some sample profiles along with their activation codes (`$AC`) and ICCIDs (`$ICCID`). The ICCID is
usually not known in advance. It becomes known after the eUICC has decrypted and installed the profile package. It
should also be noted that the ICCID parameter is always issued in its raw format (digits swapped, padded with 'F' at
the end).

In tryme.cfg one will also find a profile `X`, this profile is a placeholder in case the user decides not to edit
tryme.cfg and to pass all parameters from the commandline instead.

### Downloading And Enabling A Profile

In the following we will discuss how a profile download is triggered and how the results should look like. In the
following example we will pass all parameters directly from the commandline. It is assumed that the REST API of
onomondo-eim is available at 127.0.0.1:8080.

In the first step we will issue a download `order` using the `tryme_download.sh`. The parameter `X` tells tryme.cfg to
use the placeholder profile. The second parameter is the EID (not to be confused with ICCID) of the eUICC. The
third parameter serves as a placeholder for the ICCID, which we do not know or need yet. The last parameter is the
`activationCode`.

```
./tryme_download.sh X 12345678900000000000000000001234 NOT_NEEDED '1$rsp.example.com$EXAMPLE'
```

When the script is executed, it will `create` the related REST resource and then `lookup` the REST `resource`
periodically. As soon as the IPAd fetches the eIM package with the ProfileDownloadTriggerRequest we should see the
`status` change from `new` to `work` and when the download is done, the status should change again to `done`.

When the profile download was successful, the JSON output should look like this:

```
{"status": "done", "timestamp": "1718710695", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"download": {"activationCode": "1$rsp.example.com$EXAMPLE"}}}, "outcome": [{"profileInstallationResult": {"finalResult": "successResult", "iccid": "12324567899999911191"}}], "debuginfo": "1234...ABCD..."}
```

The profile download was successful in case the `outcome` shows a `successResult` along with the `iccid` value. This
then means that the profile is successfully installed and that we may abort the `tryme_download.sh` script now.
(to keep the rest table clean one should `delete` the rest resource now as described above)

However, the profile is not enabled yet. In order to use it, we must issue an `enable` PSMO first. To do that we may
run `tryme_enable.sh`. The parameter `X` tells tryme.cfg to use the placeholder profile again. The second parameter is
the EID and the third parameter is the ICCID that we have just taken from the JSON output above.

```
./tryme_enable.sh X 12345678900000000000000000001234 12324567899999911191
```

We now must wait again until the IPAd fetches the related eIM package with the eUICC package that contains the `enable`
PSMO. When the status reaches `done`, we should see the following JSON output:

```
{"status": "done", "timestamp": "1718710734", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"psmo": [{"enable": {"iccid": "12324567899999911191", "rollback": false}}]}}, "outcome": [{"enableResult": "ok"}], "debuginfo": "1234...ABCD..."}
```

The enableResult in the outcome shows `ok`. This means that everything went well and the profile is now enabled.

### Getting a List with Installed Profiles

The PSMO `listProfileInfo` (see also GSMA SGP.32, section 5.13.4) allows the REST API user to get a list of all
currently installed profiles, including their ICCIDs. This info is in particular valuable in case the API user must
synchronize its local records with the actual situation on the eUICC.

To send a `listProfileInfo` run:
```
./tryme_listProfileInfo.sh X 12345678900000000000000000001234
```

As soon as the IPAd has fetched and executed the related eUICC package, the JSON output should look like this:

```
{"status": "done", "timestamp": "1721306238", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"psmo": [{"listProfileInfo": {}}]}}, "outcome": [{"listProfileInfoResult": {"finalResult": "successResult", "profileInfoList": [{"iccid": "989444999999990920F3", "isdpAid": "A0000005591010FFFFFFFF8900001000", "profileState": "enabled", "serviceProviderName": "OsmocomSPN", "profileName": "TS48V1-A-UNIQUE", "profileClass": "operational"}, {"iccid": "989444999999990930F1", "isdpAid": "A0000005591010FFFFFFFF8900001100", "profileState": "disabled", "serviceProviderName": "OsmocomSPN", "profileName": "TS48V1-B-UNIQUE-nojavacard-nocsim", "profileClass": "operational"}]}}], "debuginfo": "1234...ABCD..."}
```

We can see that there are two profile installed `989444999999990920F3` and `989444999999990930F1`. We also can see that
`989444999999990920F3` is currently enabled.

### Performing an eUICC Data Request

The eUICC data Request is a special operation, that allows the eIM to request some important master data from the eUICC.
(see also GSMA SGP.32, section 2.11.1.2) This includes the EUM and the eUICC certificate that is required to
authenticate eCO and PSMO responses. The order takes a `tagList` as input, which describes what kind of data to request.
The exact meaning of the tags can be found in the aforementioned spec reference.

To perform an eUICC data request, execute the following script like so:
```
./tryme_euiccDataRequest.sh X 12345678900000000000000000001234
```

There should be response like this:
```
{"status": "done", "timestamp": "1721307295", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"edr": {"tagList": "80BF20BF228384A5A688A9BF2B"}}}, "outcome": [{"euiccDataResult": {"edrResult": "ok", "euiccData": {"euiccInfo1": "BF20618203020500A92C0414F54172BDF98A95D65CBEB88A38A1C11D800A85C30414C0BC70BA36929D43B467FF57570530E57AB8FCD8AA2C0414F54172BDF98A95D65CBEB88A38A1C11D800A85C30414C0BC70BA36929D43B467FF57570530E57AB8FCD8", "associationToken": 1, "eumCertificate": "308202783082021FA003020102020412345678300A06082A8648CE3D04030230443110300E06035504030C07546573742043493111300F060355040B0C0854455354434552543110300E060355040A0C0752535054455354310B30090603550406130249543020170D3230303430313039323833375A180F32303534303332343039323833375A3037310B300906035504061302455331153013060355040A0C0C52535020546573742045554D3111300F06035504030C0845554D20546573743059301306072A8648CE3D020106082A8648CE3D030107034200041330D59256AC0CB50BD928D0F4C68007C485FE3F42988AD3EE3875AE33F4983AB23B4DD4C31340D676DD8E11F9C5CBA1B11EB694EED0994DB529285E632C8906A382010830820104301F0603551D23041830168014F54172BDF98A95D65CBEB88A38A1C11D800A85C3301D0603551D0E04160414DD3DA24D350C1CC5D0AF0965F40EC34C5EE409F1300E0603551D0F0101FF04040302020430170603551D200101FF040D300B3009060767811201020102300E0603551D1104073005880388370530120603551D130101FF040830060101FF02010030350603551D1F042E302C302AA028A0268624687474703A2F2F63692E746573742E6578616D706C652E636F6D2F43524C2D422E63726C303E0603551D1E0101FF04343032A030302EA42C302A31153013060355040A0C0C52535020546573742045554D3111300F060355040513083839303439303332300A06082A8648CE3D040302034700304402200C567BF01E45244863AD7A4613F7572EEF3439F698B4711AA397AEEFC5445CE702206E993AA0A505F260B0EEF62CC30A2BBE453B0E8248218FD53304EF7F9074EE10", "euiccCertificate": "30820217308201BDA0030201020209020000000000000001300A06082A8648CE3D0403023037310B300906035504061302455331153013060355040A0C0C52535020546573742045554D3111300F06035504030C0845554D20546573743020170D3234303530313133313733305A180F37353030303232333133313733305A307C310B3009060355040613024445311E301C060355040A0C157379736D6F636F6D2052535020546573742045554D312930270603550405132038393034343034353131383432373438343830303030303030303031313632383122302006035504030C197379736D6F45554943432D49325420546573742065554943433059301306072A8648CE3D020106082A8648CE3D03010703420004BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DBA36B3069301D0603551D0E04160414E24191C0ECCEA8FC45926196EF1A2E53D5E6CD06301F0603551D23041830168014DD3DA24D350C1CC5D0AF0965F40EC34C5EE409F1300E0603551D0F0101FF04040302078030170603551D200101FF040D300B3009060767811201020101300A06082A8648CE3D0403020348003045022100E3BF7BAF5106E51656E315B7FED1D6F8360CA0F4D70ECB8D5AD38E159DE4A9C702203C3B11B748EC4788D8C62A6D75C13B0959CB56A8C2D68DD9C7C66C0615AACE73", "ipaCapabilities": "300B8005000001000181020001", "deviceInfo": "3008800412345678A100", "notificationsList": "A0819CBF378198BF27528010E0F1356728A44720B219A3B70C9DF57EBF2F2E800102810207800C1974657374736D6470706C7573312E6578616D706C652E636F6D5A0A989444999999990930F1060388370AA208A1068001028101095F37407FA95E72677338D953EE62395D7BCDC365BF217DFE6DAF315A98503F9DEF97103EAFBA7EDB5E16621BA14061354A75390FF0E8332B26AC31C8E2C9FAA6C07358"}}}], "debuginfo": "1234...ABCD..."}
```

Since we used the tagList "80BF20BF228384A5A688A9BF2B", which includes all possible tags to request, we get a lot of
information back. The returned data fields are in their ASN.1 encoded representation unless the requested tag consists
of a single primitive type (e.g. `associationToken`).

### Setting a Parameters in the `euicc` Table

Even though the `euicc` table is populated automatically, it may still be that the REST API user wants to adjust
certain parameters. Let's assume that we have a setup that mostly uses consumer eUICCs in an IoT emulation mode. Now we
want to add a native IoT eUICC. Let's say the card uses a root CI that we do not have configured in `sys.config` yet,
but we have the public key. Also the card has been used for experiments with PSMOs already, so the counterValue is
somewhere in the upper three digits range and not at 1 as it would be for a virgin eUICC.

In this case we would craft an order like this:
```
{ "eidValue" : "'$EID'", "order" : { "euicc": [ { "counterValue" : 1000 }, { "consumerEuicc" : false }, { "signAlgo" : "prime256v1" }, { "signPubKey" : "04BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DB" } ] } }'
```

We would edit the order into tryme_set_euicc_param.sh and run the script:
```
./tryme_set_euicc_param.sh X 12345678900000000000000000001234
```

The order will execute as an internal process. This means that no IPAd interaction is involved. However, on the REST
API the behavior will not be any different, except that the `status` will change from `new` to `done` directly. When
all changes to the `euicc` table are made accordingly, we should get a result like this:

```
{"status": "done", "timestamp": "1721309044", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"euicc": [{"counterValue": 1000}, {"consumerEuicc": false}, {"signAlgo": "prime256v1"}, {"signPubKey": "04BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DB"}]}}, "outcome": [{"euiccUpdateResult": "ok"}], "debuginfo": "836400046E6F6E65"}
```

Now the eIM will treat the eUICC as a native IoT eUICC, a proper public key set and the counter value is high enough
so that we can sign new eUICC packages correctly.
