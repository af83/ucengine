#!/usr/bin/env sh

SELF=$0
ROOT_DIR=$(dirname $SELF)

ERL=erl

MNESIA_DIR=tmp
LOG_DIR=tmp

NODE=ucengine
HOST=localhost
ERLANG_NODE=$NODE@$HOST

NAME=-name

ERL_ARGS="+K true +P 65535 +A 2 -pa ebin -run uce_app  \
          -mnesia dir ${MNESIA_DIR} -boot start_sasl  \
          -sasl sasl_error_logger {file,\"${LOG_DIR}/ucengine-sasl.log\"} \
          -kernel error_logger {file,\"${LOG_DIR}/ucengine.log\"} \
          -os_mon start_memsup false"

ERL_COMMANDS=""
PIDFILE=tmp/ucengine.pid

export ERL_LIBS=deps:/usr/lib/yaws/

run()
{
    $ERL $NAME $ERLANG_NODE $ERL_ARGS $ERL_COMMANDS
}

start()
{
    $ERL $NAME $ERLANG_NODE $ERL_ARGS -detached $ERL_COMMANDS
    echo Started
}

debug()
{
    $ERL -sname ucengine-dbg -hidden -remsh $ERLANG_NODE
}

stop()
{
    kill -15 $(cat $PIDFILE)
    echo Stopped
}

tests()
{
    $ERL $NAME $ERLANG_NODE $ERL_ARGS -noshell -eval 'tests:start().'
}

internal_cmd()
{
    exec $ERL			\
	-pa "ebin/"		\
	-hidden			\
	-noinput		\
	-sname uce_ctl_$$	\
	-s uce_ctl		\
	-nodename uce_ctl	\
	-object "$1"		\
	-action "$2"		\
	-dummy			\
	"$@"
}

case $1 in
    run) run;;
    start) start;;
    debug) debug;;
    restart) stop; start;;
    stop) stop;;
    tests) tests;;
    org) internal_cmd $@;;
    meeting) internal_cmd $@;;
    --help) internal_cmd $@;;
    demo) internal_cmd $@;;
esac
