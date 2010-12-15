-module(meeting_controller).

-export([init/0, add/3, list/3, get/3, update/3, leave/3, join/3, roster/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Meetings",
		method='GET',
		regexp="/meeting/([^/]+)/([^/]+)",
		callbacks=[{?MODULE, list, [], [], [], []}]},
     
     #uce_route{module="Meetings",
		method='GET',
		regexp="/meeting/([^/]+)/all/([^/]+)",
		callbacks=[{?MODULE, get, [], [], [], []}]},
		   
     #uce_route{module="Meetings",
		method='PUT',
		regexp="/meeting/([^/]+)/all/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "start", "end", "metadata"],
			    [required, 0, ?NEVER_ENDING_MEETING, []],
			    [string, integer, integer, dictionary],
			    [user, any, any, any]}]},
     
     #uce_route{module="Meetings",
		method='POST',
		regexp="/meeting/([^/]+)/all/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, update,
			    ["uid", "start", "end", "metadata"],
			    [required, 0, ?NEVER_ENDING_MEETING, []],
			    [string, integer, integer, dictionary],
			    [user, any, any, any]}]},
     
     #uce_route{module="Roster",
		method='PUT',
		regexp="/meeting/([^/]+)/all/([^/]+)/roster/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, join,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},
     
     #uce_route{module="Roster",
		method='DELETE',
		regexp="/meeting/([^/]+)/all/([^/]+)/roster/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, leave,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},
     
     #uce_route{module="Roster",
		method='GET',
		regexp="/meeting/([^/]+)/all/([^/]+)/roster",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, roster,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]}].

add([Org, Name], [Uid, Start, End, Metadata], _) ->
    case uce_acl:check(Uid, "meeting", "add", [Org, ""], [{"name", Name}]) of
	{ok, true} ->
	    case uce_meeting:add(#uce_meeting{id=[Org, Name],
					      start_date=Start,
					      end_date=End,
					      metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, created} ->
		    json_helpers:created()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

update([Org, Name], [Uid, Start, End, Metadata], _) ->
    case uce_acl:check(Uid, "meeting", "update", [Org, ""], [{"name", Name}]) of
	{ok, true} ->
	    case uce_meeting:update(#uce_meeting{id=[Org, Name],
						 start_date=Start,
						 end_date=End,
						 metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

list([Org, Status], [], _) ->
    case uce_meeting:list(Org, Status) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Meetings} ->
	    json_helpers:json({array, [meeting_helpers:to_json(Meeting) || Meeting <- Meetings]})
    end.

get(Location, [], _) ->
    case uce_meeting:get(Location) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Meeting} ->
	    json_helpers:json(meeting_helpers:to_json(Meeting))
    end.

join([Org, Meeting, To], [Uid], _) ->
    case uce_acl:check(Uid, "roster", "add", [Org, Meeting], []) of
	{ok, true} ->
	    case uce_meeting:join([Org, Meeting], To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    uce_event:add(#uce_event{type="internal.roster.add",
					     location=[Org, Meeting],
					     from=To}),
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

leave([Org, Meeting, To], [Uid], _) ->
    case uce_acl:check(Uid, "roster", "delete", [Org, Meeting], []) of
	{ok, true} ->
	    case uce_meeting:leave([Org, Meeting], To) of
		{error, Reason} ->
		    {error, Reason};
		{ok, updated} ->
		    uce_event:add(#uce_event{type="internal.roster.delete",
					     location=[Org, Meeting],
					     from=To}),
		    json_helpers:ok()
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

roster(Location, [Uid], _) ->
    case uce_acl:check(Uid, "roster", "list", Location, []) of
	{ok, true} ->
	    case uce_meeting:roster(Location) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Roster} ->
		    FullRoster =
			lists:map(fun(MemberUid) ->
					  case uce_user:get(MemberUid) of
					      {ok, User} ->
						  {struct, [{uid, User#uce_user.uid},
							    {auth, User#uce_user.auth},
							    {metadata, {struct, User#uce_user.metadata}}]};
					      {error, _} ->
						  []
					  end
				  end,
				  Roster),
		    json_helpers:json({array, FullRoster})
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
