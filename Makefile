PROJECT = eid
PROJECT_DESCRIPTION = Unique ID generator
PROJECT_VERSION = 0.5.0

# Test dependencies.
TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck 0.8.13

CT_LOGS_DIR = ct_logs

include erlang.mk

# Generate rebar.config on build.
app:: rebar.config

