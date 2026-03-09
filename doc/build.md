### Building and Running

The build process is managed using `rebar3`. There are several commands that
will build the application for various cases:

```
$ rebar3 shell
```

For development will build source and dependencies, start the servers, and drop
into a live repl shell.

```
$ rebar3 compile
```

Will compile output into a target `_build` directory subfolder.

```
$ rebar3 release
```

Will compile, link, and package a fully self contained artifact folder that is
suitable for deployment.

All commands can be preceded by a profile specification like so:

```
$ rebar3 as [PROFILE_NAME] [CMD]
```

Profiles are arbitrary (except for `default` and `test`) and are defined in
[`rebar.config`](../rebar.config)

So, for example:

```
$ rebar3 as dev shell
```

Will build using the `dev` profile, which is defined to use the
[`config/sys.dev.config`](../config/sys.dev.config), which disables TLS for
local development.

As part of this process, rebar3 will precompile `asn1/*.asn1` files into
`asn1_gen/*.[asn1db|erl]` outputs. This is a build artifact that is used by the
erlang compiler proper to link into the main application. In point of fact
erlang's first class asn1 support is a driving factor for its use here.

Usually build artifacts land in `_build`, but it simplifies the erlang
compiler/linker step to have this output in tree. It is gitignored and should
not be modified directly.

Some additional commands:

```
rebar3 clean

```

Removes artifacts in the _build directory for the specified profile or default.
For a "full clean": `rm -rf _build asn1_gen`

```
rebar3 ct
```

Builds and runs [Common Test](https://www.erlang.org/doc/apps/common_test/basics_chapter.html)

The overall process may take a while since erlang.mk will also take care of
downloading and compiling further erlang related dependencies. The build
process can also be started with `make run`. This will start the application
immediately after finishing the compilation.

A typical output after the first startup

```
$ rebar3 as dev shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling unicode_util_compat
===> Compiling mimerl
===> Compiling quickrand
===> Compiling ssl_verify_fun
===> Compiling parse_trans
===> Compiling metrics
===> Compiling idna
===> Compiling certifi
===> Compiling hackney
===> Compiling ranch
===> Compiling jiffy
===> Compiling uuid
===> Compiling cowlib
===> Compiling cowboy
===> Compiling c_src/decoder.c
===> Compiling c_src/encoder.c
===> Compiling c_src/jiffy.c
===> Compiling c_src/termstack.c
===> Compiling c_src/utf8.c
===> Compiling c_src/util.c
===> Compiling c_src/doubles.cc
===> Compiling c_src/objects.cc
===> Compiling c_src/double-conversion/bignum-dtoa.cc
===> Compiling c_src/double-conversion/bignum.cc
===> Compiling c_src/double-conversion/cached-powers.cc
===> Compiling c_src/double-conversion/diy-fp.cc
===> Compiling c_src/double-conversion/double-conversion.cc
===> Compiling c_src/double-conversion/fast-dtoa.cc
===> Compiling c_src/double-conversion/fixed-dtoa.cc
===> Compiling c_src/double-conversion/strtod.cc
===> Linking eim/_build/default/lib/jiffy/priv/jiffy.so
===> Generating parser for: "eim/asn1/DERSignature.asn1"
===> Generating parser for: "eim/asn1/PEDefinitions.asn1"
===> Generating parser for: "eim/asn1/PKIX1Explicit88.asn1"
===> Generating parser for: "eim/asn1/PKIX1Implicit88.asn1"
===> Generating parser for: "eim/asn1/RSPDefinitions.asn1"
===> Generating parser for: "eim/asn1/SGP32Definitions.asn1"
===> Analyzing applications...
===> Compiling eim
Erlang/OTP 28 [erts-16.2] [source] [64-bit] [smp:11:11] [ds:11:11:10] [async-threads:1] [jit]

Eshell V16.2 (press Ctrl+G to abort, type help(). for help)
=NOTICE REPORT==== 27-Feb-2026::11:06:29.299400 ===
eIM! version:0.0.1

=INFO REPORT==== 27-Feb-2026::11:06:29.304942 ===
    application: mnesia
    exited: stopped
    type: temporary

=NOTICE REPORT==== 27-Feb-2026::11:06:29.314147 ===
    mnesia database schema created

=NOTICE REPORT==== 27-Feb-2026::11:06:29.332451 ===
    mnesia started

=NOTICE REPORT==== 27-Feb-2026::11:06:29.339866 ===
    rest table created

=NOTICE REPORT==== 27-Feb-2026::11:06:29.340919 ===
    work table created

=NOTICE REPORT==== 27-Feb-2026::11:06:29.341927 ===
    euicc table created

=NOTICE REPORT==== 27-Feb-2026::11:06:29.342849 ===
Starting ESipa HTTP server at {127,0,0,1}:8000...

=NOTICE REPORT==== 27-Feb-2026::11:06:29.359201 ===
Starting REST HTTP server at {127,0,0,1}:8080...

===> Booted sasl
===> Booted mnesia
===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted unicode_util_compat
===> Booted idna
===> Booted mimerl
===> Booted certifi
===> Booted syntax_tools
===> Booted parse_trans
===> Booted ssl_verify_fun
===> Booted metrics
===> Booted hackney
===> Booted xmerl
===> Booted jiffy
===> Booted quickrand
===> Booted uuid
===> Booted onomondo_eim
===> Booted runtime_tools
1>
```

Note that here the shell is open after boot.

(To run onomondo-eim under the control of `systemd`, the example service file
/contrib/onomondo-eim.service may be used)

To verify that the ESipa interface is reachable it can be probed using a
web-browser. In the example above, the ESipa interface runs on 127.0.0.1, port
8000 and uses SSL. It can be probed by typing "https://127.0.0.1:8000/" into
the address line of a web-browser. The result should be the string "eIM
working!". The REST interface can be probed with the same method (see section
`eIM Information`).

Configuration
-------------

The user relevant configuration files can be found in the `./config` directory.
In the default configuration onomondo-eim will start with all services bound to
local host. The most relevant configuration file is `sys.config`, which sets
the application parameters. The file `vm.args` sets erlang VM parameters. In
most cases, those parameters do not have to be modified.

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
