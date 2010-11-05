-module(mnesia_db).

-author('victor.goya@af83.com').

-export([init/1,
	 terminate/0]).

-include("uce.hrl").

init(_) ->
    uce_acl_mnesia:init(),
    uce_user_mnesia:init(),
    uce_meeting_mnesia:init(),
    uce_org_mnesia:init(),
    uce_file_mnesia:init(),
    uce_event_mnesia:init(),
    uce_presence_mnesia:init(),
    ok.

terminate() ->
    ok.
