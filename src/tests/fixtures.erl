-module(fixtures).

-include("uce.hrl").

-export([setup/0, teardown/1]).

setup() ->
    setup_meetings(),
    setup_users(),
    setup_events(),
    setup_testers().

teardown(Testers) ->
    teardown_meetings(),
    teardown_testers(Testers),
    teardown_users(),
    teardown_solr(),
    ok.

setup_meetings() ->
    teardown_meetings(),
    Now = utils:now(),
    uce_meeting:add(#uce_meeting{id=["testmeeting"],
                                 metadata=[{"description", "Meeting"}],
                                 start_date=Now,
                                 end_date=?NEVER_ENDING_MEETING}),
    uce_meeting:add(#uce_meeting{id=["closedmeeting"],
                                 metadata=[{"description", "Meeting"}],
                                 start_date=Now,
                                 end_date=Now}),
    uce_meeting:add(#uce_meeting{id=["upcomingmeeting"],
                                 metadata=[{"description", "Meeting"}],
                                 start_date=2569256203952,
                                 end_date=?NEVER_ENDING_MEETING}),
    uce_meeting:add(#uce_meeting{id=["testmeeting"],
                                 metadata=[{"description", "Meeting"}],
                                 start_date=Now,
                                 end_date=?NEVER_ENDING_MEETING}),
    ok.

teardown_meetings() ->
    uce_meeting:delete(["newmeeting"]),
    uce_meeting:delete(["testmeeting"]),
    uce_meeting:delete(["closedmeeting"]),
    uce_meeting:delete(["upcomingmeeting"]),
    uce_meeting:delete(["testmeeting"]),
    ok.

setup_users() ->
    teardown_users(),

    uce_user:add(#uce_user{uid="participant.user@af83.com",
			   auth="password",
			   credential="pwd"}),
    uce_acl:add(#uce_acl{uid="participant.user@af83.com",
			 action="add",
			 object="presence"}),
    uce_acl:add(#uce_acl{uid="participant.user@af83.com",
			 action="delete",
			 object="presence",
			 conditions=[{"user", "participant.user@af83.com"}]}),

    uce_user:add(#uce_user{uid="anonymous.user@af83.com", auth="anonymous", credential=""}),
    uce_acl:add(#uce_acl{uid="anonymous.user@af83.com",
			 action="add",
			 object="presence"}),
    uce_acl:add(#uce_acl{uid="anonymous.user@af83.com",
			 action="delete",
			 object="presence",
			 conditions=[{"user", "anonymous.user@af83.com"}]}),

    uce_user:add(#uce_user{uid="token.user@af83.com", auth="token", credential="4444"}),

    ok.
teardown_users() ->
    uce_user:delete("participant.user@af83.com"),
    uce_acl:delete("participant.user@af83.com", "add", "presence", [], []),
    uce_acl:delete("participant.user@af83.com", "delete", "presence", [""], [{"user", "participant.user@af83.com"}]),

    uce_user:delete("anonymous.user@af83.com"),
    uce_acl:delete("anonymous.user@af83.com", "add", "presence", [], []),
    uce_acl:delete("anonymous.user@af83.com", "delete", "presence", [""], [{"user", "anonymous.user@af83.com"}]),

    uce_user:delete("test.user@af83.com"),
    ok.

setup_events() ->
    uce_event:add(#uce_event{ type="test_event_1",
			      location=["testmeeting"],
			      from="participant.user@af83.com"}),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_2",
			      location=["testmeeting"],
			      from="user_2"
			    }),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_3",
			      location=["testmeeting"],
			      from="user_3",
			      metadata=[{"description", "test"}]
			    }),

    uce_event:add(#uce_event{ type="test_event_1",
			      location=["testmeeting"],
			      from="participant.user@af83.com"
			    }),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_2",
			      location=["testmeeting"],
			      from="participant.user@af83.com"
			    }),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_3",
			      location=["testmeeting"],
			      from="participant.user@af83.com",
			      metadata=[{"description", "test"}]
			    }),
    ok.

setup_testers() ->
    RootUid = "root.user@af83.com",
    {ok, created} = uce_user:add(#uce_user{uid=RootUid,
                                           auth="password",
                                           credential="pwd"}),
    {ok, created} = uce_acl:add(#uce_acl{uid=RootUid,
                                         action="all",
                                         object="all"}),
    {ok, RootSid} = uce_presence:add(#uce_presence{uid=RootUid,
                                                   auth="password",
                                                   metadata=[]}),
    UglyUid = "ugly.user@af83.com",
    {ok, created} = uce_user:add(#uce_user{uid=UglyUid,
                                           auth="password",
                                           credential="pwd"}),
    {ok, UglySid} = uce_presence:add(#uce_presence{uid=UglyUid,
						   auth="password",
						   metadata=[]}),
    [{RootUid, RootSid}, {UglyUid, UglySid}].

teardown_testers([{RootUid, RootSid}, {UglyUid, UglySid}]) ->
    uce_user:delete(RootUid),
    uce_acl:delete(RootUid, "all", "all", [""], []),
    uce_presence:delete(RootSid),

    uce_user:delete(UglyUid),
    uce_presence:delete(UglySid),
    ok.

teardown_solr() ->
    uce_event_solr_search:delete("*:*"),
    ok.
