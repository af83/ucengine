#!/bin/sh

CWD=$(dirname $0)

UCENGINE_ADMIN="$CWD/ucengine-admin"

HOST=$1

if [ -z $HOST ];
then
    echo "You must provide a domain. $0 <domain>"
    exit 1
fi

$UCENGINE_ADMIN $HOST meeting add "demo"  --name "Demonstration room" --description "U.C.Engine demonstration room"
$UCENGINE_ADMIN $HOST meeting add "demo2" --name "Another room"       --description "Another test room"

# Role participant
$UCENGINE_ADMIN $HOST role add participant
$UCENGINE_ADMIN $HOST role access add participant "add" "presence"
$UCENGINE_ADMIN $HOST role access add participant "add" "roster"
$UCENGINE_ADMIN $HOST role access add participant "get" "meeting"
$UCENGINE_ADMIN $HOST role access add participant "list" "meeting"
$UCENGINE_ADMIN $HOST role access add participant "view" "video"
$UCENGINE_ADMIN $HOST role access add participant "all" "event"

# Role speaker
$UCENGINE_ADMIN $HOST role add speaker

# Role owner
$UCENGINE_ADMIN $HOST role add owner
$UCENGINE_ADMIN $HOST role access add owner "update" "meeting"
$UCENGINE_ADMIN $HOST role access add owner "add" "user.role" --role "speaker"
$UCENGINE_ADMIN $HOST role access add owner "delete" "user.role" --role "speaker"

# Users
USERS="participant participant2 participant3 owner"

for user in $USERS
do
  echo "Create user $user"
  $UCENGINE_ADMIN $HOST user add $user "password" "pwd"
  $UCENGINE_ADMIN $HOST user role add $user "participant"
done

$UCENGINE_ADMIN $HOST user role add "owner" "owner" --location "demo"
$UCENGINE_ADMIN $HOST user role add "owner" "owner" --location "demo2"

# anonymous
$UCENGINE_ADMIN $HOST user add "anonymous" "none" ""
