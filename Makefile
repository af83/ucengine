VERSION       = 0.0.1
CC            = erlc
ERL           = erl
EBIN          = ebin
CFLAGS        = -Iinclude +warn_unused_vars +warn_unused_import

MNESIA_DIR    = tmp
LOG_DIR       = tmp
ERL_ARGS      = -mnesia dir $(MNESIA_DIR) -boot start_sasl
    ERL_ARGS += -sasl sasl_error_logger '{file, "$(LOG_DIR)/ucengine-sasl.log"}'
    ERL_ARGS += -kernel error_logger '{file, "$(LOG_DIR)/ucengine.log"}'
    ERL_ARGS += -os_mon start_memsup false
ERL_COMMANDS  = -eval 'demo:start()'

PIDFILE       = tmp/ucengine.pid

DIRS          = ebin datas/files

ERL_FILES     = $(shell find src/models/behaviours src -type f -and -name "*.erl" -exec basename '{}' \;)
BEAM_TARGETS  = $(ERL_FILES:.erl=.beam)
BEAM_TARGETS := $(addprefix ebin/, $(BEAM_TARGETS))

APP_FILES     = $(shell find src -type f -and -name "*.app" -exec basename '{}' \;)
APP_TARGETS  := $(addprefix ebin/, $(APP_FILES))

export ERL_LIBS := $(ERL_LIBS):deps

all: compile

$(DIRS):
	mkdir -p $(DIRS)

###############################################################################
# Build
###############################################################################
compile: $(DIRS) $(BEAM_TARGETS) $(APP_TARGETS)
	(cd deps/emongo && make)

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
	@erl +K true +P 65535 +A 2 -name ucengine -pa ebin -run uce_app \
        $(ERL_ARGS) $(ERL_COMMANDS)
start: compile
	@erl +K true +P 65535 +A 2 -name ucengine -pa ebin -run uce_app \
        $(ERL_ARGS) -detached $(ERL_COMMANDS)
	@echo Started
stop:
	-@kill -15 $(shell cat $(PIDFILE))
	@echo Stopped
restart: stop start

tests: compile
	@erl +K true +P 65535 +A 2 -noshell -name ucengine -pa ebin -run uce_app \
        $(ERL_ARGS) -eval 'tests:start().'

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

