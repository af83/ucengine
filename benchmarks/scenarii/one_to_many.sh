#!/usr/bin/env bash

UCENGINE_ADMIN="ucengine-admin"

HOST=$1

if [ -z $HOST ];
then
    echo "You must provide a domain. $0 <domain>"
    exit 1
fi

MEETINGS="demo demo2"

for meeting in $MEETINGS
do
    echo "Creating meeting $meeting"
    $UCENGINE_ADMIN $HOST meeting add $meeting
done

# Role participant
$UCENGINE_ADMIN $HOST role add participant
$UCENGINE_ADMIN $HOST role access add participant "add" "presence"
$UCENGINE_ADMIN $HOST role access add participant "get" "infos"
$UCENGINE_ADMIN $HOST role access add participant "add" "roster"
$UCENGINE_ADMIN $HOST role access add participant "get" "meeting"
$UCENGINE_ADMIN $HOST role access add participant "list" "meeting"
$UCENGINE_ADMIN $HOST role access add participant "view" "video"
$UCENGINE_ADMIN $HOST role access add participant "all" "event"


FILE="users.csv"

# Users
rm -f $FILE
for id in $(seq 1 1200)
do
  user="participant_$id"
  $UCENGINE_ADMIN $HOST user add $user "password" "pwd"
  $UCENGINE_ADMIN $HOST user role add $user "participant"
  echo "$user;pwd" >> $FILE
done
