-module(fixtures).

-include("uce.hrl").

-export([setup/0, teardown/1]).

setup() ->
    setup_org(),
    setup_meetings(),
    setup_users(),
    setup_events(),
    {ROOT_UID, ROOT_SID} = setup_root(),
    {ROOT_UID, ROOT_SID}.

teardown(ROOT_INFOS) ->
    teardown_org(),
    teardown_meetings(),
    teardown_root(ROOT_INFOS),
    teardown_users(),
    ok.

setup_org() ->
    teardown_org(),
    uce_org:add(#uce_org{name="testorg", metadata=[{"description", "testorg"}]}),
    ok.
teardown_org() ->
    uce_org:delete("testorg"),
    uce_org:delete("neworg"),
    ok.

setup_meetings() ->
    teardown_meetings(),
    uce_meeting:add(#uce_meeting{id=["testorg", "testmeeting"],
				     metadata=[{"description", "Meeting"}],
				     start_date=utils:now(),
				     end_date=?NEVER_ENDING_MEETING}),
    Now = utils:now(),
    uce_meeting:add(#uce_meeting{id=["testorg", "closedmeeting"],
				     metadata=[{"description", "Meeting"}],
				     start_date=Now,
				     end_date=Now}),
    uce_meeting:add(#uce_meeting{id=["testorg", "upcomingmeeting"],
				     metadata=[{"description", "Meeting"}],
				     start_date=2569256203952,
				     end_date=?NEVER_ENDING_MEETING}),
    uce_meeting:add(#uce_meeting{id=["otherorg", "testmeeting"],
				     metadata=[{"description", "Meeting"}],
				     start_date=utils:now(),
				     end_date=?NEVER_ENDING_MEETING}),
    ok.
teardown_meetings() ->
    uce_meeting:delete(["testorg", "newmeeting"]),
    uce_meeting:delete(["testorg", "testmeeting"]),
    uce_meeting:delete(["testorg", "closedmeeting"]),
    uce_meeting:delete(["testorg", "upcomingmeeting"]),
    uce_meeting:delete(["otherorg", "testmeeting"]),
    ok.

setup_users() ->
    teardown_users(),
    
    uce_user:add(#uce_user{uid="root.user@af83.com",
			   auth="password",
			   credential="pwd"}),
    uce_acl:add(#uce_acl{uid="root.user@af83.com",
			 action="all",
			 object="all",
			 conditions=[]}),
    
    uce_user:add(#uce_user{uid="participant.user@af83.com",
			   auth="password",
			   credential="pwd"}),
    uce_acl:add(#uce_acl{uid="participant.user@af83.com",
			 action="add",
			 object="presence",
			 conditions=[{"org", "testorg"}]}),
    uce_acl:add(#uce_acl{uid="participant.user@af83.com",
			 action="delete",
			 object="presence",
			 conditions=[{"user", "participant.user@af83.com"}]}),
    
    uce_user:add(#uce_user{uid="anonymous.user@af83.com", auth="anonymous", credential=""}),
    uce_acl:add(#uce_acl{uid="anonymous.user@af83.com",
			 action="add",
			 object="presence",
			 conditions=[{"org", "testorg"}]}),
    uce_acl:add(#uce_acl{uid="anonymous.user@af83.com",
			 action="delete",
			 object="presence",
			 conditions=[{"user", "anonymous.user@af83.com"}]}),
    
    uce_user:add(#uce_user{uid="token.user@af83.com", auth="token", credential="4444"}),
    
    ok.
teardown_users() ->
    uce_user:delete("root.user@af83.com"),
    uce_acl:delete("root.user@af83.com", "all", "all", ["", ""], []),
    
    uce_user:delete("participant.user@af83.com"),
    uce_acl:delete("participant.user@af83.com", "add", "presence", ["testorg"], []),
    uce_acl:delete("participant.user@af83.com", "delete", "presence", ["", ""], [{"user", "participant.user@af83.com"}]),
    
    uce_user:delete("anonymous.user@af83.com"),
    uce_acl:delete("anonymous.user@af83.com", "add", "presence", ["testorg"], []),
    uce_acl:delete("anonymous.user@af83.com", "delete", "presence", ["", ""], [{"user", "anonymous.user@af83.com"}]),
    
    uce_user:delete("test.user@af83.com"),
    ok.

setup_events() ->
    uce_event:add(#uce_event{ type="test_event_1",
			      location=["testorg", "testmeeting"],
			      from="user_1"}),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_2",
			      location=["testorg", "testmeeting"],
			      from="user_2"
			    }),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_3",
			      location=["testorg", "testmeeting"],
			      from="user_3",
			      metadata=[{"description", "test"}]
			    }),
    
    uce_event:add(#uce_event{ type="test_event_1",
			      location=["otherorg", "testmeeting"],
			      from="user_1"
			    }),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_2",
			      location=["otherorg", "testmeeting"],
			      from="user_1"
			    }),
    timer:sleep(10),
    uce_event:add(#uce_event{ type="test_event_3",
			      location=["otherorg", "testmeeting"],
			      from="user_1",
			      metadata=[{"description", "test"}]
			    }),
    ok.

setup_root() ->
    Uid = "root.user@af83.com",
    ESid = uce_presence:add(#uce_presence{uid=Uid,
					  org="testorg",
					  auth="password",
					  metadata=[]}),
    {Uid, ESid}.
teardown_root({_, ROOT_SID}) ->
    ok = uce_presence:delete(ROOT_SID),
    ok.

teardown_solr() ->
    solr:delete("test_solr_event"),
    ok.
