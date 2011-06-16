DIRS          = rel/ucengine/data/files

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

demo: $(DIRS)
	-@rm rel/ucengine/priv/ -fr
	-@cp -r priv rel/ucengine/.

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

dialyze: compile
	./rebar skip_deps=true check-plt
	./rebar skip_deps=true dialyze

###############################################################################
# Benchmark
###############################################################################

bench:
	mkdir -p ebin/
	erlc -o ebin/ tsung/tsung_utils.erl
	mkdir -p benchmarks/results
	./utils/benchmark $(SCENARIO)
	rm -rf ebin
	./utils/fetchmetrics `pwd`/`find  benchmarks/results -name "20*" | sort -r | head -1`

###############################################################################
# Cleanup
###############################################################################
clean:
	-@rm -v erl_crash.dump -f
	./rebar clean

.PHONY: clean bench
