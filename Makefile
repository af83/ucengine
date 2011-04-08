DIRS          = data/files

all: compile

$(DIRS):
	mkdir -p $(DIRS)

###############################################################################
# Build
###############################################################################
compile: $(DIRS)
	./rebar get-deps
	./rebar compile

rel: compile
	./rebar generate

###############################################################################
# Usual targets
###############################################################################
dev: cleanrel rel

run: dev
	rel/ucengine/bin/ucengine console

start: dev
	rel/ucengine/bin/ucengine start

stop:
	rel/ucengine/bin/ucengine stop

restart: dev
	rel/ucengine/bin/ucengine restart

tests: dev
	rel/ucengine/bin/ucectl tests
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
