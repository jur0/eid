
PROJECT = eid
ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
    +warn_shadow_vars +warn_obsolete_guard

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

.PHONY: all clean-all app clean test deps clean-deps build-plt dialyze

all: app

clean-all: clean clean-deps
	rm -rf .$(PROJECT).plt $(DEPS_DIR) logs

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

app: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
        | sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
        > ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERL_OPTS) -I include/ -o ebin/ $?

clean:
	rm -rf ebin/ test/*.beam erl_crash.dump

##### Tests

CT_RUN = ct_run \
        -noshell \
        -pa ebin/ $(DEPS_DIR)/*/ebin/ \
        -dir test \
        -logdir logs

deps: $(DEPS_DIR)/meck
	@$(MAKE) -C $(DEPS_DIR)/meck

clean-deps:
	-@$(MAKE) -C $(DEPS_DIR)/meck clean

$(DEPS_DIR)/meck:
	@mkdir -p $(DEPS_DIR)
	git clone -n -- https://github.com/eproxus/meck.git $(DEPS_DIR)/meck
	cd $(DEPS_DIR)/meck ; git checkout -q master

test: clean clean-deps deps app
	@mkdir -p logs/
	$(CT_RUN) -suite eid_SUITE

##### Dialyzer

build-plt: app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
        --apps erts kernel stdlib

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native \
        -Werror_handling -Wrace_conditions -Wunmatched_returns -Wunderspecs
