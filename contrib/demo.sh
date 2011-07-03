#!/bin/sh

UCENGINE_ADMIN="ucengine-admin"

HOST=$1
echo "infos update"
$UCENGINE_ADMIN $HOST infos update --description "U.C.Engine is a publish/subscribe server with persistence. It allows you to build real time applications like collaboration based services, live meetings, games or anything that fits well in an event driven philosophy." --htags "ucengine" --logo "ucengine.png"
echo "meeting add demo"
$UCENGINE_ADMIN $HOST meeting add "demo"  --name "Demonstration room" --description "U.C.Engine demonstration room"
echo "meeting add demo2"
$UCENGINE_ADMIN $HOST meeting add "demo2" --name "Another room"       --description "Another test room"
echo "role add participant"
# Role participant
$UCENGINE_ADMIN $HOST role add participant
echo "role access add participant 'add' 'presence'"
$UCENGINE_ADMIN $HOST role access add participant "add" "presence"
echo "role access add participant 'get' 'infos'"
$UCENGINE_ADMIN $HOST role access add participant "get" "infos"
echo "role access add participant 'add' 'roster'"
$UCENGINE_ADMIN $HOST role access add participant "add" "roster"
echo "role access add participant 'get' 'meeting'"
$UCENGINE_ADMIN $HOST role access add participant "get" "meeting"
echo "role access add participant 'list' 'meeting'"
$UCENGINE_ADMIN $HOST role access add participant "list" "meeting"
echo "role access add participant 'view' 'video'"
$UCENGINE_ADMIN $HOST role access add participant "view" "video"
echo "role access add participant 'all' 'event'"
$UCENGINE_ADMIN $HOST role access add participant "all" "event"

# Role speaker
echo "role add speaker"
$UCENGINE_ADMIN $HOST role add speaker

# Role owner
echo "role add owner"
$UCENGINE_ADMIN $HOST role add owner
echo "role access add owner 'update' 'meeting'"
$UCENGINE_ADMIN $HOST role access add owner "update" "meeting"
echo 'role access add owner "add" "user.role" --role "speaker"'
$UCENGINE_ADMIN $HOST role access add owner "add" "user.role" --role "speaker"
echo 'role access add owner "delete" "user.role" --role "speaker"'
$UCENGINE_ADMIN $HOST role access add owner "delete" "user.role" --role "speaker"

# Users
USERS="participant participant2 participant3 owner"

for user in $USERS
do
  echo 'user add $user "password" "pwd"'
  $UCENGINE_ADMIN $HOST user add $user "password" "pwd"
  echo 'user role add $user "participant"'
  $UCENGINE_ADMIN $HOST user role add $user "participant"
done

echo 'user role add "owner" "owner" --location "demo"'
$UCENGINE_ADMIN $HOST user role add "owner" "owner" --location "demo"
echo 'user role add "owner" "owner" --location "demo2"'
$UCENGINE_ADMIN $HOST user role add "owner" "owner" --location "demo2"

# anonymous
echo 'user add "anonymous" "none" ""'
$UCENGINE_ADMIN $HOST user add "anonymous" "none" ""
