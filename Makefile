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

###############################################################################
# Usual targets
###############################################################################
run: compile
	bin/ucectl run

start: compile
	bin/ucectl start

stop:
	bin/ucectl stop

restart:
	bin/ucectl restart

tests: compile
	bin/ucectl tests
	./rebar eunit

###############################################################################
# Cleanup
###############################################################################
.PHONY: clean
.PHONY: deepclean
clean:
	-@rm -v tmp/* -fr
	-@rm -v data/* -fr
	-@rm -v erl_crash.dump -f

deepclean: clean
	./rebar clean
