#!/usr/bin/env bash

SELF=$0
ROOT_DIR=$(dirname $SELF)

ERL=erl

MNESIA_DIR=tmp
LOG_DIR=tmp

ERL_ARGS="+K true +P 65535 +A 2 -name ucengine -pa ebin -run uce_app  \
          -mnesia dir ${MNESIA_DIR} -boot start_sasl  \
          -sasl sasl_error_logger {file,\"${LOG_DIR}/ucengine-sasl.log\"} \
          -kernel error_logger {file,\"${LOG_DIR}/ucengine.log\"} \
          -os_mon start_memsup false"

ERL_COMMANDS=" -eval demo:start()"
PIDFILE=tmp/ucengine.pid

run()
{
    $ERL $ERL_ARGS $ERL_COMMANDS
}

start()
{
    $ERL $ERL_ARGS -detached $ERL_COMMANDS
    echo Started
}

stop()
{
    kill -15 $(cat $PIDFILE)
    echo Stopped
}

tests()
{
    $ERL $ERL_ARGS -noshell -eval 'tests:start().'
}

case $1 in
    run) run;;
    start) start;;
    restart) stop; start;;
    stop) stop;;
    tests) tests;;
esac
