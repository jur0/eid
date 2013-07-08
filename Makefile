# general options
PROJECT = eid
V = 1

# test dependencies
TEST_DEPS = meck
dep_meck = https://github.com/eproxus/meck.git 0.7.2

# test options
CT_SUITES = eid

include erlang.mk
