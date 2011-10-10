#!/usr/bin/env bash

#
# Populate U.C.Engine before a bench
#

UCENGINE_ADMIN="ucengine-admin"
NB_MEETINGS=150 # Number of meetings
NB_USERS=10     # Number of user *per meeting*
FILE="users.csv"

HOST=$1

if [ -z $HOST ];
then
    echo "You must provide a domain. $0 <domain>"
    exit 1
fi

which $UCENGINE_ADMIN
if [[ $? -ne 0 ]];
then
    echo "$UCENGINE_ADMIN was not found"
    exit 2
fi

# Role participant
$UCENGINE_ADMIN $HOST role add participant
$UCENGINE_ADMIN $HOST role access add participant "add" "presence"
$UCENGINE_ADMIN $HOST role access add participant "get" "infos"
$UCENGINE_ADMIN $HOST role access add participant "add" "roster"
$UCENGINE_ADMIN $HOST role access add participant "get" "meeting"
$UCENGINE_ADMIN $HOST role access add participant "list" "meeting"
$UCENGINE_ADMIN $HOST role access add participant "view" "video"
$UCENGINE_ADMIN $HOST role access add participant "all" "event"

rm -f $FILE

for i in $(seq 1 $NB_MEETINGS)
do
    MEETING="meeting_$i"
    echo "Creating meeting $MEETING"
    $UCENGINE_ADMIN $HOST meeting add $MEETING


    # Users
    for j in $(seq 1 $NB_USERS)
    do
        j=$((($i - 1) * $NB_USERS + $j))
        USER="user_$j"
        echo "Creating user $USER"
        $UCENGINE_ADMIN $HOST user add $USER password toto
        $UCENGINE_ADMIN $HOST user role add $USER "participant"
        echo "$MEETING;$USER;toto" >> $FILE
    done
done
