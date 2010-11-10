-module(meeting_controller).

-export([init/0, add/3, list/3, get/3, update/3, leave/3, join/3, roster/3]).

-include("uce.hrl").

init() ->
    [#uce_route{module="Meetings",
		method='GET',
		regexp="/meeting/([^/]+)/([^/]+)",
		callbacks=[{?MODULE, list, [], [], []}]},
     
     #uce_route{module="Meetings",
		method='GET',
		regexp="/meeting/([^/]+)/all/([^/]+)",
		callbacks=[{?MODULE, get, [], [], []}]},
		   
     #uce_route{module="Meetings",
		method='PUT',
		regexp="/meeting/([^/]+)/all/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, add,
			    ["uid", "start", "end", "metadata"],
			    [required, 0, ?NEVER_ENDING_MEETING, []],
			    [string, integer, integer, dictionary]}]},
     
     #uce_route{module="Meetings",
		method='POST',
		regexp="/meeting/([^/]+)/all/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, update,
			    ["uid", "start", "end", "metadata"],
			    [required, 0, ?NEVER_ENDING_MEETING, []],
			    [string, integer, integer, dictionary]}]},
     
     #uce_route{module="Roster",
		method='PUT',
		regexp="/meeting/([^/]+)/all/([^/]+)/roster/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, join,
			    ["uid"],
			    [required],
			    [string]}]},
     
     #uce_route{module="Roster",
		method='DELETE',
		regexp="/meeting/([^/]+)/all/([^/]+)/roster/([^/]+)",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, leave,
			    ["uid"],
			    [required],
			    [string]}]},
     
     #uce_route{module="Roster",
		method='GET',
		regexp="/meeting/([^/]+)/all/([^/]+)/roster",
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string]},
			   {?MODULE, roster,
			    ["uid"],
			    [required],
			    [string]}]}].

add(Location, [EUid, Start, End, Metadata], _) ->
    case uce_acl:check(EUid, "meeting", "add", Location, []) of
	true ->
	    case uce_meeting:add(#uce_meeting{id=Location,
					      start_date=Start,
					      end_date=End,
					      metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:created()
	    end;
	false ->
	    {error, unauthorized}
    end.

update(Location, [EUid, Start, End, Metadata], _) ->
    case uce_acl:check(EUid, "meeting", "update", Location, []) of
	true ->
	    case uce_meeting:update(#uce_meeting{id=Location,
						 start_date=Start,
						 end_date=End,
						 metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.

list([Org, Status], [], _) ->
    case uce_meeting:list(Org, Status) of
	{error, Reason} ->
	    {error, Reason};
	Meetings ->
	    json_helpers:json({array, [meeting_helpers:to_json(Meeting) || Meeting <- Meetings]})
    end.

get(Location, [], _) ->
    case uce_meeting:get(Location) of
	{error, Reason} ->
	    {error, Reason};
	Meeting ->
	    json_helpers:json(meeting_helpers:to_json(Meeting))
    end.

join([Org, Meeting, To], [EUid], _) ->
    case uce_acl:check(EUid, "roster", "add", [Org, Meeting], []) of
	true ->
	    case uce_meeting:join([Org, Meeting], To) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    uce_event:add(#uce_event{type="internal.roster.add",
					     location=[Org, Meeting],
					     from=To}),
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.

leave([Org, Meeting, To], [EUid], _) ->
    case uce_acl:check(EUid, "roster", "delete", [Org, Meeting], []) of
	true ->
	    case uce_meeting:leave([Org, Meeting], To) of
		{error, Reason} ->
		    {error, Reason};
		ok ->
		    uce_event:add(#uce_event{type="internal.roster.delete",
					     location=[Org, Meeting],
					     from=To}),
		    json_helpers:ok()
	    end;
	false ->
	    {error, unauthorized}
    end.

roster(Location, [EUid], _) ->
    case uce_acl:check(EUid, "roster", "list", Location, []) of
	true ->
	    case uce_meeting:roster(Location) of
		{error, Reason} ->
		    {error, Reason};
		Roster ->
		    FullRoster =
			lists:map(fun(MemberEUid) ->
					  case uce_user:get(MemberEUid) of
					      User when is_record(User, uce_user) ->
						  {struct, [{uid, User#uce_user.uid},
							    {auth, User#uce_user.auth},
							    {metadata, {struct, User#uce_user.metadata}}]};
					      _ ->
						  nothing
					  end
				  end,
				  Roster),
		    json_helpers:json({array, FullRoster})
	    end;
	false ->
	    {error, unauthorized}
    end.
