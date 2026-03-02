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
    * erlang
    * rebar3

A `.tool-versions` file is included for version specification, and recommended to be used with [asdf](https://asdf-vm.com/guide/getting-started.html)

TODO: fill out plugin installation instructions.

* Dependencies required to compile and run the restop.py tool
```
  apt-get install python3-full
  pip3 install erlang-py
  pip3 install requests
```

TODO: virtualize this env etc requirements.txt.

Documentation
-------------

* [Build](doc/build.md)
* [REST API](doc/rest_api.md)
* [Database](doc/database.md)
* [Examples](doc/examples.md)
