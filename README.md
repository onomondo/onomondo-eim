# onomondo-eim
onomondo-eim is an erlang based eIM (Remote provisioning and management of the eUICC in IoT Devices, see also SGP.31)
implementation. Besides an ES9+ (SMDP+) and an ESipa (IPAd) interface it also features a REST API that allows an API
user to perform management tasks on the eUICC (profile download, execution of PSMOs and eCOs).

Interfaces
----------

### ESipa

The ESipa interface of onomondo-eim is implemented as an HTTP server interface (see also GSMA SGP.32, section 6.1) that
uses ASN.1 function bindings (see also GSMA SGP.32, section 6.3. The HTTP client (`IPAd`) on the remote end is expected to
establish the connection with the ESipa.GetEimPackage request and keep it open at least until the related procedure
ends with the ESipa.HandleNotifiations request (see also GSMA SGP.32, section 3.1.1.1, figure 5)

### ES9+

The ES9+ interface of onomondo-eim is implemented as a HTTP client interface (see also GSMA SGP.22, section 5.6). that
uses the JSON function bindings (see also GSMA SGP.22, section 6.5).

### REST API

The REST API is a custom interface that is used by the eIM operator to manage his fleet of eUICCs. It is implemented
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

Once the application is running it will display a prompt. To exit the application type `CTRL+G` and then `q`.

(To run onomondo-eim under the control of `systemd`, the example service file /contrib/onomondo-eim.service may be
used)


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
* `rest_timeout_stuck`: Configure timeout until an order/procedure (e.g. profile download) must finish.
* `rest_timeout_noshow`: Configure timeout until an order/procedure must start.
* `rest_timeout_expired`: Configure timeout until the REST API user must lookup/delete the order via the REST API

### vm.args

* See also: https://www.erlang.org/doc/man/erl.html
* mnesia dir: configure the location of the mnesia database. The default setting will store the database in
  `./_rel/onomondo_eim_release/db`.

#### Timeout Behavior

The three REST API related timeouts (`rest_timeout_`) serve the purpose that the underlying REST database of the REST
API won't overflow over time in case REST API users fail to monitor their orders and most importantly, delete their
orders when done.

* The timeout `rest_timeout_stuck` gurds against stuck orders. Orders may get stuck due to communication errors between
  SMDP+ or IPAd. When a procedure is stuck for too long it gets marked as done and an appropriate error code is
  communicated to the REST API user. Since a procedure usually won't take more than a few minutes (usually below one
  minute) 300 sec. would be a good compromise here.

* The timeout `rest_timeout_noshow` guards against IPAd/eUICCs that fail to show up. When an order is placed the IPAd
  is expected to poll the eIM within a reasonable amount of time. When the IPAd fails to poll for some reason, the
  order gets marked as done and an appropriate error code is communicated to the REST API user. Usually the polling
  is triggered through some side channel or the polling happens periodically. Depending on the situation the timeout may
  be set to several hours or even days. In any case it should not be lower than `rest_timeout_stuck` for obvious
  reasons. A recommended timeout value would be 1800 (30 min).

* The timeout `rest_timeout_expired` guards against careless REST API users. As mentioned already, the REST API user is
  expected to carefully monitor his orders and delete them on his own responsibility. However, it may be that a REST
  API user fails to monitor an order he created. To prevent the database from gradually overflowing, such orders will
  be expired. This means they are deleted silently. Depending on the situation and the quality of the tooling that
  operates the REST API, this timeout can be set generously (hours, days or even weeks). In any case, the timeout should
  not be set lower than `rest_timeout_noshow` for obvious reasons. A recommended timeout value would be 86400 (24h)


REST API
--------

The REST API offered by onomondo-eim is a powerful interface to manage a fleet of eUICCs. The REST API lets the user
trigger profile downloads and offers full access to all PSMOs and ECOs that are specified in GSMA SGP.32.

### Facilities

The REST API is divided up into so called `facilities`. The `facility` identifier is the first path element of the HTTP
URL. There are four facilities defined:

* `download`: management of profile downloads
* `psmo`: Profile State Management Operations (PSMO)
* `eco`: Eim Configuration Operations (eCO)
* `euicc`: eim-local eUICC configuration Operations

The purpose of the `facilities` is to provide separation at URL level. This allows for easier filtering to restrict
access for specific REST API users.

### Operations

The REST API defines four different basic operations. The name of the operation is the second path element of the HTTP URL:

* `create`: create a rest resource, returns resourceId
* `lookup`: lookup a rest resource by its resourceId, returns JSON
* `delete`: delete a rest resource by its resourceId, returns JSON
* `list`: list all resource IDs available for the current facility, returns JSON.

### ResourceId

When a REST resource is created, a so called 'resourceId' is returned by the REST API. The resourceId uniquely
identifies the REST resource and has to be memorized by the REST API user to perform further operations.

Example: URL with selected facility "download" and operation "lookup" on the resourceId "8a901bd9-f203-4eae-bcba-12dee32f4444"
```
http://127.0.0.1:8080/download/lookup/8a901bd9-f203-4eae-bcba-12dee32f4444
```

### Interface

The REST API receives JSON formatted data via HTTP POST requests and returns a JSON formatted response. The following
chapter describes how the REST interface works in general.

#### Creating Orders

Each request contains a so called `resource`. The resource is a JSON formatted request that contains the `eidValue` and
a so called `order`. The `eidValue` identifies the eUICC and the `order` contains specific parameters that the eIM needs
to fulfill a specific task (e.g. trigger a profile download).

Example: Rest resource that orders to trigger a profile download
```
{ "eidValue" : "89882119900000000000000000000005", "order" : {"activationCode" : "1$rsp.example.com$EXAMPLE"}}'
```

#### Monitoring Orders

The REST-API user is expected to poll the rest resource from time to time to check on its `status`. The polling is done
using the lookup operation:

Example: URL to lookup a specific REST resource
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
{"status": "done", "timestamp": "1718881629", "resource": {"eidValue": "89086030202200000022000027485428", "order": {"download": {"activationCode": "1$rsp.example.com$EXAMPLE"}}}, "outcome": [{"profileInstallationResult": {"finalResult": "successResult", "iccid": "984474680000730771F0"}}], "debuginfo": "83680264001370656E64696E674E6F74696669636174696F6E680264001970
```

Example: A typical `lookup` response when an invalid `resourceId` is used:
```
{"status": "absent"}
```

#### Deleting REST resources

The REST API user is responsible to keep the `rest` table clean. When an `order` is done or has to be revoked for some
reason, the REST API user will use the `delete` operation to remove the related REST resource from the `rest` table.

Example: URL to `delete` a specific REST resource
```
http://127.0.0.1:8080/download/delete/8a901bd9-f203-4eae-bcba-12dee32f4444
```

The eIM will confirm the deletion of the REST resource by responding with a status `deleted`.

Example: A typical delete response
```
{"status":"deleted"}
```

In case the API user forgets to delete the REST resource for some reason, the REST resource it will not stay in the
database indefinitely. After some generous timeout, the eIM will automatically delete the REST resource from the REST
table. However, the intension behind this mechanism is to guard against database memory leaks and is not intended to be
used as the general mechanism to get rid of REST resources that are no longer used.

#### Listing REST resources

The REST API user has to keep track of his REST resources. However, there may be circumstances where the REST API user
must recover his state. In this case it is possible to list all `resourceIds` of all REST resources that are currently
in the `rest` table.

Example: URL to list all REST resources available in the `download` `facility`
```
http://127.0.0.1:8080/download/list
```

The eIM will respond with a `resourceIdList`. The REST API user may then use this list to lookup the state of each
REST resource. Since the list only contains the `resourceIds` of the current `facility`, the REST API user must repeat
the process for all `facilities`.

Example: A typical `list` response with three `resourceIDs`
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
configuration on the eUICC so that the eUICC is associated with the current onomondo-eim instance. The eUICC will
create an `associationToken` in response, which should be passed back to the eIM via the REST API on the `euicc`
facility. However, on a virgin eUICC the `associationToken` will be 1, which is also the default onomondo-eim will use.
(see also GSMA SGP.32, section 3.5.2.1)

* `eimConfigurationData`: This data format has the same contents as `addInitialEimRequest`, but withut the envelope.
This format is suitable to be used for adding the eIM to the eUICC remotely via an eCO. (see also GSMA SGP.32, section
3.5.1.1)

Example: URL that returns eIM information
```
http://127.0.0.1:8080/
```

#### JSON Schema

This description above describes only the basic concept of the REST API. To give a system integrator a detailed overview
on how the requests/responses should look like two JSON schema files are shipped with onomondo-eim:

* contrib/rest_api_resource_schema.json: This schema describes a REST resource. This is the format that the requests issued
to the REST API should have.

* contrib/rest_api_response_schema.json: This schema that describes the JSON formatted response that is received from the
REST API when the `lookup` operation is performed.

* contrib/rest_api_info_schema.json: This schema that describes the JSON formatted info page that is received when the
REST API is called without any parameters.

### Tyring Out The REST API

The REST API is complex interface that is difficult to operate out of the box without any prior familiarization. To
give a system integrator a good starting point, the contrib directory contains "tryme-scripts" that serve as examples
and an easy way try out the REST API right away.

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

The file also defines some sample profiles along with their activation codes ($AC) and ICCIDs ($ICCID). The ICCID is
usually not known in advance. It becomes known after the eUICC has decrypted and installed the profile package. It
should also be noted that the ICCID parameter is always issued in its raw format (digits swapped, padded with 'F' at
the end).

In tryme.cfg one will also find a profile 'X', this profile is a placeholder in case the user decides not to edit
tryme.cfg and to pass all parameters from the commandline instead.

#### Downloading And Enabling A Profile

In the following we will discuss how a profile download is triggered and how the results should look like. In the
following example we will pass all parameters directly from the commandline. It is assumed that the REST API of
onomondo-eim is avaliable at 127.0.0.1:8080.

In the first step we will issue a download `order` using the `tryme_download.sh`. The parameter X tells tryme.cfg to
use the placeholder profile. The second parameter is the EID (not to be confused with ICCID) of the eUICC. The
third parameter serves as a placeholder for the ICCID, which we do not know or need yet. The last parameter is the
`activationCode`.

```
./tryme_download.sh X 12345678900000000000000000001234 NOT_NEEDED '1$rsp.example.com$EXAMPLE'
```

When the script is executed, it will `create` the related REST resource and then `lookup` the REST resource
periodically. As soon as the IPAd fetches the eIM package with the ProfileDownloadTriggerRequest we should see the
`status` change from `new` to `work` and when the download is done, the status should change again to `done`.

When the profile download was successful, the JSON output should look like this:

```
{"status": "done", "timestamp": "1718710695", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"download": {"activationCode": "1$rsp.example.com$EXAMPLE"}}}, "outcome": [{"profileInstallationResult": {"finalResult": "successResult", "iccid": "12324567899999911191"}}]
```

The profile download was successful in case the outcome shows a `successResult` along with the `iccid` value. This
then means that the profile is successfully installed and that we may abort the `tryme_download.sh` script now.
(to keep the rest table clean one should `delete` the rest resource now as described above)

However, the profile is not enabled yet. In order to use it, we must issue an `enable` PSMO first. To do that we may
run `tryme_enable.sh`. The parameter X tells tryme.cfg to use the placeholder profile again. The second parameter is
the EID and the third parameter is the ICCID that we have just taken from the JSON output above.

```
./tryme_enable.sh X 12345678900000000000000000001234 12324567899999911191
```

We now must wait again until the IPAd fetches the related eIM package with the eUICC package that contains the `enable`
PSMO. When the status reaches `done`, we should see the following JSON output:

```
{"status": "done", "timestamp": "1718710734", "resource": {"eidValue": "12345678900000000000000000001234", "order": {"psmo": [{"enable": {"iccid": "12324567899999911191", "rollback": false}}]}}, "outcome": [{"enableResult": "ok"}]
```

The enableResult in the outcome shows `ok`. This means that everything went well and the profile is now enabled.


Database
--------

To keep state, onomondo-eim uses an mnesia database. The database files are automatically created on the first startup.
The database contains three tables. `rest`, `work` and `euicc`.

### rest Table

In this table the rest resources are stored along with their status information. This is also the table that the REST
API user is maintaining the REST resources in. The column names are similar to the field names used on the REST API
interface.

* `resourceId`: the `resourceId` used to identify REST resources. The database automatically assigns a unique
  `resourceId` and returns it to the REST API user.
* `facility`: defines the `facilitiy` to which the REST resource is associated.
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

### work Table

The `status` in the `rest` table is changed from `new` to `work` as soon as the related procedure begins to execute.
To maintain the state of the specific procedure the `work` table is used. The table is kept in RAM only, which means
that its contents are volatile. This is a performance optimization. The work state may be updated quite frquently
and in case the eIM has to handle a lot of IPAd clients simultaniously it is better not to synchronize
each state change to the disk. The state in the `work` table is also only valuable during the execution of the
procedure, a restart of the eIM while a procedure is running forcefully terminate a procedure in any case.

The entries in the `work` table are always associated to a specific erlang process, which in particular is the
child process the HTTP server creates when the connection is made. It is important that the HTTP client keeps
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
* `transactionId`: the `transactioId` of the ESipa request
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

### euicc Table

The `euicc` table contains the eUICC master data. This is in particular the EID and other meta information that is
required for the eIM to operate. The data in this table is usually automatically collected and updated by the eIM.
However, there may be situations where the REST API user wants to update certain parameters of the `euicc` table.
This is why the REST API allows to modify the contents of the `euicc` table via `orders` from the `euicc` `facility`

The `euicc` table maintains the following columns:

* `eidValue`: the EID of the related eUICC
* `counterValue`: the conter value that is used when signing eUICC packages
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
