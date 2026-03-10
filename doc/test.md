Currently the unit test setup is limited: a basic harness and a few assertion
tests, using the [Common
Test](https://www.erlang.org/doc/apps/common_test/basics_chapter.html)
framework that ships with Erlang/OTP


Run tests using:

```
$ rebar3 ct
```

Test runs can be limited in scope:

```
rebar3 ct --suite onomondo_eim_SUITE --case esipa_minimal_asn1_request
```

A coverage report can be generated with

```
$ rebar3 ct --cover
```

...and then...

```
$ rebar3 cover
```

Will generate a nice html view of it.

`rebar3` and/or Common Test does not support file watching, a cross platform
alternative is [`watchexec`](https://github.com/watchexec/watchexec)

ex watching `.erl` files and rerunning tests:

```
$ watchexec -e erl rebar3 ct
```
