-module(fixtures).

-include("uce.hrl").

-export([setup/0, teardown/1]).

setup() ->
    setup_meetings(),
    setup_users(),
    setup_testers().

teardown(_Testers) ->
    apply(list_to_atom(atom_to_list(config:get(db)) ++ "_db"), drop, []),
    teardown_solr(),
    ok.

setup_meetings() ->
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

setup_users() ->
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

teardown_solr() ->
    uce_event_solr_search:delete("*:*"),
    ok.
