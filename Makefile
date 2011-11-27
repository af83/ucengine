DIRS          = rel/ucengine/data/files

# Try to find yaws include dir for Debian/Ubuntu users
# If you have a custom yaws install, you can change it with for example:
# make rel ERL_LIBS=/usr/local/lib/yaws
ifeq ($(shell test -d /usr/lib/yaws && echo found),found)
ERL_LIBS:=/usr/lib/yaws
$(warning Found directory /usr/lib/yaws)
$(warning Using ERL_LIBS=${ERL_LIBS})
export ERL_LIBS
endif

all: compile

$(DIRS):
	mkdir -p $(DIRS)

###############################################################################
# Build
###############################################################################
compile:
	./rebar get-deps
	./rebar compile

rel: compile
	./rebar generate force=1

###############################################################################
# Usual targets
###############################################################################
dev: rel $(DIRS)

wwwroot: $(DIRS)
	-@rm rel/ucengine/wwwroot/ -fr
	-@cp -r wwwroot rel/ucengine/.

run: dev
	rel/ucengine/bin/ucengine console

start: dev
	rel/ucengine/bin/ucengine start

stop:
	rel/ucengine/bin/ucengine stop

restart: dev
	rel/ucengine/bin/ucengine restart

tests: dev
	rel/ucengine/bin/ucengine-admin tests
	./rebar skip_deps=true eunit

mnesia_tests: dev
	sed -i  's/db, mongodb/db, mnesia/' rel/ucengine/etc/uce.cfg
	rel/ucengine/bin/ucengine-admin tests
	./rebar skip_deps=true eunit

mongodb_tests: dev
	sed -i  's/db, mnesia/db, mongodb/' rel/ucengine/etc/uce.cfg
	sed -i  's/database, "ucengine"}/database, "ucengine_test"},{index, 0}/' rel/ucengine/etc/uce.cfg
	rel/ucengine/bin/ucengine-admin tests
	./rebar skip_deps=true eunit


build-plt:
	dialyzer --build_plt --output_plt .ucengine_dialyzer.plt \
	--apps kernel stdlib sasl crypto

dialyze: compile
	dialyzer -r ucengine --plt .ucengine_dialyzer.plt

###############################################################################
# Benchmark
###############################################################################

populate:
	./benchmarks/scenarii/$(SCENARIO).sh localhost

bench:
	@mkdir -p benchmarks/ebin/
	@erlc -o benchmarks/ebin/ benchmarks/tsung_utils.erl
	@mkdir -p benchmarks/results
	@./utils/benchmark $(SCENARIO) $(LEVEL)
	@rm -rf benchmarks/ebin

###############################################################################
# Cleanup
###############################################################################
clean:
	-@rm -v erl_crash.dump -f
	./rebar clean

.PHONY: clean bench
