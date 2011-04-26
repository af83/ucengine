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
	./rebar eunit

###############################################################################
# Cleanup
###############################################################################
.PHONY: clean
.PHONY: deepclean
.PHONY: cleanrel
clean:
	-@rm -v tmp/* -fr
	-@rm -v data/* -fr
	-@rm -v erl_crash.dump -f

cleanrel:
	rm -rf rel/ucengine

deepclean: clean cleanrel
	./rebar clean
