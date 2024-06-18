# onomondo-eim
Remote provisioning and management of the eUICC in IoT Devices as defined in eSIM IoT Architecture and Requirements SGP.31.


Getting Started
---------------

### Installing run time dependencies

* Dependencies required to compile and run the onomondo_eim erlang application:
  apt-get install erlang
  apt-get install curl
  apt-get install build-essential

* Dependencies required to compile and run the restop.py tool
  apt-get install python3-full
  pip3 install erlang-py
  pip3 install requests


### Building and Running

The build process is started using `make`. The overall process may take a while since erlang.mk will also take care of
downloading and compiling further erlang related dependencies. The build process can also be started with `make run`.
This will start the application immediately after finishing the compilation.

Once the application is running it will display a prompt. To exit the applicatin type `CTRL+G` and then `q`.


Configuration
-------------

The user relevant configuration files can be found in the `./config` directory. In the default configuration
onomondo-eim will start with all services bound to local host. The most relevant configuration file is
`sys.config`, which sets the application parameters. The file `vm.args` sets erlang VM parameters. In most
cases those parameters do not have to be modified.

### sys.config

* esipa_ip: Configure on which network interface the ESipa interface should listen.
* esipa_port: Configure the port number where the ESipa interface should listen.
* esipa_ssl_disable: Set this to true to use HTTP instead of HTTPs. This is a debug feature intended for lab setups to
  simplify the creation of protocol traces.
* esipa_ssl_cert: Configure the location of the SSL certificate.
* esipa_ssl_key: Configure the location of the SSL private key.
* rest_ip: Configure on which network interface the REST API interface should listen.
* rest_port: Configure the port number where the REST API interface should listen.
* eim_id: Configure the eimId of the eIM instance.
* es9p_ssl_disable: Set this to true to use HTTP instead of HTTPs. This is a debug feature intended for lab setups to
  simplify the creation of protocol traces.
* eim_cert: Configure the location of the certificate that is used for verification of eUICC packages.
* eim_key: Configure the location of the private key that is used for signing eUICC packages.
* counter_value: Set the start value of the replay protection counter (eUICC packages).
* rest_timeout_stuck: Configure timeout until an order/procedure (e.g. profile download) must finish.
* rest_timeout_noshow: Configure timeout until an order/procedure must start.
* rest_timeout_expired: Configure timeout until the REST API user must lookup/delete the order via the REST API

#### timeout recomendations

The three REST API related timeouts (rest_timeout_) serve the purpose that the underlying REST database of the REST
API won't overflow over time in case REST API users fail to monitor their orders and most importantly delete their
orders when done.

* The timeout rest_timeout_stuck gurds against stuck orders. Orders may get stuck due to communication errors between
  SMDP+ or IPAd. When a procedure is stuck for too long it gets marked as done and an appropriate error code is
  communicated to the REST API user. Since the overall time a procedure usually won't take more than a few minutes
  (usually below one minute) 300 sec. would be a good compromise here.

* The timeout rest_timeout_noshow guards against IPAd/eUICCs that fail to show up. When an order is placed the IPAd
  is expected to poll the eIM within a reasonable amount of time. When the IPAd fails to poll for some reason the
  order gets marked as done and an appropriate error code is communicated to the REST API user. Usually the polling
  is triggered through some side channel or the polling happens regularly. Depending on the situation the timeout may
  be set to several hours or even days. In any case it must not be lower than rest_timeout_stuck for obvious reasons.
  A recommended timeout value would be 1800 (30 min).

* The timeout rest_timeout_expired guards against careless REST API users. As mentioned already, the REST API user is
  expected to carefully monitor his orders and delete them on his own responsibility. However, it may be that a REST
  API user fails to monitor an order he created. To prevent the database from gradually overflowing such orders will
  be expired. This means they are deleted silently. Depending on the situation and the quality of the tooling that
  operates the REST API, this timeout can be set generously (hours, days or even weeks). In any case the timeout must
  not be set lower than rest_timeout_noshow for obvious reasons. A recommended timeout value would be 86400 (24h)

### vm.args

* See also: https://www.erlang.org/doc/man/erl.html
* mnesia dir: configure the location of the mnesia database. The default setting will store the dabase in
  `./_rel/onomondo_eim_release/db`.

