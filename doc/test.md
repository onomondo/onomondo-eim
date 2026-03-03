recommended watchexec: cross platform
```
$ watchexec -e erl rebar3 ct --suite onomondo_eim_SUITE --case esipa_minimal_asn1_request
```

coverage can be generated with

```
$ rebar3 ct --cover
```

and then

```
$ rebar3 cover
```

will create a nice html report.

also optional:

```
diff --git a/rebar.config b/rebar.config
index 43b3505..957f4cd 100644
--- a/rebar.config
+++ b/rebar.config
@@ -16,7 +16,7 @@

 {provider_hooks, [{pre, [{compile, {asn1, compile}}]}]}.

-{ct_opts, [{sys_config, "config/sys.test.config"}]}.
+{ct_opts, [{sys_config, "config/sys.test.config"}, {cover, true}]}.

 {profiles, [
     {dev, [
```
