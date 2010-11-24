-module(demo).

-export([start/0]).

-include("uce.hrl").


start() ->
    uce_org:add(#uce_org{name="af83",
			     metadata=[{"description", "af83média specializes in digital communication. Our mission is to design and manage online content and communities."},
				       {"logo", "af83.png"},
				       {"htags", "af83"}]}),
    
    uce_meeting:add(#uce_meeting{id=["af83", "demo"],
				     metadata=[{"description", "UCEngine demo meetup"},
					       {"video", "http://localhost/dev/ucengine/test"}],
				     start_date=utils:now(),
				     end_date=?NEVER_ENDING_MEETING}),
    uce_event:add(#uce_event{type="twitter.hashtag.add",
			     location=["af83", "demo"],
			     from="ucengine",
			     metadata=[{"hashtag", "#TED"}]}),
    uce_event:add(#uce_event{type="twitter.hashtag.add",
			     location=["af83", "demo"],
			     from="ucengine",
			     metadata=[{"hashtag", "#sinek"}]}),
    uce_event:add(#uce_event{type="twitter.hashtag.add",
			     location=["af83", "demo"],
			     from="ucengine",
			     metadata=[{"hashtag", "#simonsinek"}]}),
    uce_event:add(#uce_event{type="twitter.hashtag.add",
			     location=["af83", "demo"],
			     from="ucengine",
			     metadata=[{"hashtag", "#ucengine"}]}),
    uce_event:add(#uce_event{type="twitter.hashtag.add",
			     location=["af83", "demo"],
			     from="ucengine",
			     metadata=[{"hashtag", "#ucengine"}]}),

    uce_meeting:add(#uce_meeting{id=["af83", "demo2"],
				     metadata=[{"description", "Meeting R&D"},
					       {"video", "/test"}],
				     start_date=utils:now(),
				     end_date=?NEVER_ENDING_MEETING}),
    uce_meeting:add(#uce_meeting{id=["af83", "agoroom"],
				     metadata=[{"description", "Meeting agoroom"},
					       {"video", "http://localhost/dev/ucengine/test"}],
				     start_date=1287738533649,
				     end_date=1287739733649}),
    
    user_controller:add(["thierry.bomandouki@af83.com"], ["password", "pwd", []], []),
    user_controller:add(["victor.goya@af83.com"], ["password", "pwd", []], []),
    user_controller:add(["louis.ameline@af83.com"], ["password", "pwd", []], []),
    user_controller:add(["alexandre.eisenchteter@af83.com"], ["password", "pwd", []], []),
    user_controller:add(["romain.gauthier@af83.com"], ["password", "pwd", []], []),
    user_controller:add(["participant"], ["password", "pwd", []], []),

    ok = feed(),

    case utils:get(config:get(admin), [uid, auth, credential]) of
	[Uid, Auth] ->
	    Sid = uce_presence:add(#uce_presence{uid=Uid, 
						 org="af83",
						 auth=Auth,
						 metadata=[]}),
	    io:format("Admin: ~p/~p~n", [Uid, Sid]);
	_ ->
	    io:format("No admin account~n")
    end,
    ok.


feed([]) ->
    ok;
feed([Path|Paths]) ->
    ["config", "samples", Org, Meeting, File] = re:split(Path, "/", [{return, list}]),
    event_helpers:feed(Path, [{"location", [Org, Meeting]}]),
    feed(Paths).

feed() ->
    Paths = filelib:wildcard("config/samples/*/*/*.json"),
    feed(Paths).

