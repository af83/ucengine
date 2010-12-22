-module(demo).

-export([start/0]).

-include("uce.hrl").


start() ->
    uce_meeting:add(#uce_meeting{id=["demo"],
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

    case utils:get(config:get(admin), [uid, auth]) of
	[Uid, Auth] ->
	    {ok, Sid} = uce_presence:add(#uce_presence{uid=Uid, 
						       auth=Auth,
						       metadata=[]}),
	    io:format("Admin: ~p/~p~n", [Uid, Sid]);
	Reason ->
	    io:format("No admin account (~p)~n", [Reason])
    end,
    ok.


feed([]) ->
    ok;
feed([Path|Paths]) ->
    ["config", "samples", Meeting, _File] = re:split(Path, "/", [{return, list}]),
    event_helpers:feed(Path, [{"location", [Meeting]}]),
    feed(Paths).

feed() ->
    Paths = filelib:wildcard("config/samples/*/*/*.json"),
    feed(Paths).

