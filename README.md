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

### vm.args

* See also: https://www.erlang.org/doc/man/erl.html
* mnesia dir: configure the location of the mnesia database. The default setting will store the dabase in
  `./_rel/onomondo_eim_release/db`.
