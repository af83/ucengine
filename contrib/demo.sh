#!/bin/sh

UCENGINE_ADMIN="ucengine-admin"

$UCENGINE_ADMIN localhost infos update --description "U.C.Engine is a publish/subscribe server with persistence. It allows you to build real time applications like collaboration based services, live meetings, games or anything that fits well in an event driven philosophy." --htags "ucengine" --logo "ucengine.png"

$UCENGINE_ADMIN localhost meeting add "demo"  --name "Demonstration room" --description "U.C.Engine demonstration room"
$UCENGINE_ADMIN localhost meeting add "demo2" --name "Another room"       --description "Another test room"

# Role participant
$UCENGINE_ADMIN localhost role add participant
$UCENGINE_ADMIN localhost role access add participant "add" "presence"
$UCENGINE_ADMIN localhost role access add participant "get" "infos"
$UCENGINE_ADMIN localhost role access add participant "add" "roster"
$UCENGINE_ADMIN localhost role access add participant "get" "meeting"
$UCENGINE_ADMIN localhost role access add participant "list" "meeting"
$UCENGINE_ADMIN localhost role access add participant "view" "video"
$UCENGINE_ADMIN localhost role access add participant "all" "event"

# Role speaker
$UCENGINE_ADMIN localhost role add speaker

# Role owner
$UCENGINE_ADMIN localhost role add owner
$UCENGINE_ADMIN localhost role access add owner "update" "meeting"
$UCENGINE_ADMIN localhost role access add owner "add" "user.role" --role "speaker"
$UCENGINE_ADMIN localhost role access add owner "delete" "user.role" --role "speaker"

# Users
USERS="participant participant2 participant3 owner"

for user in $USERS
do
  $UCENGINE_ADMIN localhost user add $user "password" "pwd"
  $UCENGINE_ADMIN localhost user role add $user "participant"
done

$UCENGINE_ADMIN localhost user role add "owner" "owner" --location "demo"
$UCENGINE_ADMIN localhost user role add "owner" "owner" --location "demo2"

# anonymous
$UCENGINE_ADMIN localhost user add "anonymous" "none" ""
