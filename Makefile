VERSION       = 0.0.1
CC            = erlc
EBIN          = ebin
CFLAGS        = -Iinclude +warn_unused_vars +warn_unused_import

DIRS          = ebin datas/files

ERL_FILES     = $(shell find src/models/behaviours src -type f -and -name "*.erl" -exec basename '{}' \;)
BEAM_TARGETS  = $(ERL_FILES:.erl=.beam)
BEAM_TARGETS := $(addprefix ebin/, $(BEAM_TARGETS))

APP_FILES     = $(shell find src -type f -and -name "*.app" -exec basename '{}' \;)
APP_TARGETS  := $(addprefix ebin/, $(APP_FILES))

all: compile

$(DIRS):
	mkdir -p $(DIRS)

###############################################################################
# Build
###############################################################################
compile: $(DIRS) $(BEAM_TARGETS) $(APP_TARGETS)
	(cd deps/emongo && make)
	(cd deps/ibrowse && make)

ebin/amqp_pubsub.beam: src/backends/pubsub/amqp/amqp_pubsub.erl
ifdef WITH_AMQP
	@echo "AMQP support enabled"
	erlc -pa ebin -W $(CFLAGS) -o ebin $<
else
	@echo "AMQP support disabled, set the WITH_AMQP environnement variable to 'yes' to enable it."
endif

ebin/%.beam: src/%.erl
	erlc -pa ebin -W $(CFLAGS) -o ebin $<
ebin/%.beam: src/*/%.erl
	erlc -pa ebin -W $(CFLAGS) -o ebin $<
ebin/%.beam: src/*/*/%.erl
	erlc -pa ebin -W $(CFLAGS) -o ebin $<
ebin/%.beam: src/*/*/*/%.erl
	erlc -pa ebin -W $(CFLAGS) -o ebin $<

ebin/%.app: src/%.app
	@cp -v $< $@
ebin/%.app: src/*/%.app
	@cp -v $< $@
ebin/%.app: src/*/*/%.app
	@cp -v $< $@

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

###############################################################################
# Cleanup
###############################################################################
.PHONY: clean
.PHONY: deepclean
clean:
	-@rm -v tmp/* -fr
	-@rm -v datas/* -fr
	-@rm -v erl_crash.dump -f
deepclean: clean
	@rm -v ebin/* -fr

