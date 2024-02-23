PROJECT = onomondo_eim
PROJECT_DESCRIPTION = eUICC IoT Remote Manager
PROJECT_VERSION = 1.0.0

ERLC_ASN1_OPTS = +der +maps

DEPS = cowboy hackney jiffy quickrand uuid
LOCAL_DEPS = mnesia
dep_cowboy_commit = 2.10.0
dep_hackney_commit = 1.20.1

DEP_PLUGINS = cowboy

BUILD_DEPS += relx
include erlang.mk
