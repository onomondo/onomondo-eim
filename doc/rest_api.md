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

