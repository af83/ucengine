-module(uce_meeting).

-author('victor.goya@af83.com').

-export([
	 add/1,

	 delete/1,

	 update/1,

	 get/1,

	 list/2,

	 join/2,
	 leave/2,
	 roster/1
	]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_meeting{} = Meeting) ->
    case ?MODULE:get(Meeting#uce_meeting.id) of
	{error, not_found} ->
	    ?DBMOD:add(Meeting);
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    {error, conflict}
    end.

delete(#uce_meeting{} = Meeting) ->
    case ?DBMOD:delete(Meeting) of
	{error, Reason} ->
	    {error, Reason};
	ok ->
	    ok
    end;
delete(Id) ->
    case ?MODULE:get(Id) of
	{error, Reason} ->
	    {error, Reason};
	Meeting ->
	    ?MODULE:delete(Meeting)
    end.


get(Id) ->
    ?DBMOD:get(Id).

update(#uce_meeting{} = Meeting) ->
    case ?MODULE:get(Meeting#uce_meeting.id) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    ?DBMOD:update(Meeting)
    end.

%% List meeting
%% @params [{"org", orgname:string}, {"status", meetingstatus:string}]
list(Org, Status) ->
    case uce_org:get(Org) of
	{error, Reason} ->
	    {error, Reason};
	_ ->
	    Now = utils:now(),
	    if
		Status == "all"; Status=="upcoming"; Status=="opened"; Status=="closed" ->
		    lists:filter(fun(#uce_meeting{start_date=Start, end_date=End}) ->
					 case Status of
					     "all" ->
						 true;
					     "upcoming" ->
						 Now < Start;
					     "opened" ->
						 case Now >= Start of
						     true ->
							 if					     
							     End == ?NEVER_ENDING_MEETING -> true;
							     Now =< End -> true;
							     true -> false
							 end;
						     false ->
							 false
						 end;
					     "closed" ->
						 if
						     End == ?NEVER_ENDING_MEETING -> false;
						     Now >= End -> true;
						     true -> false
						 end;
					     _ ->
						 false
					 end
				 end,
				 ?DBMOD:list(Org));
		true ->
		    {error, bad_parameters}
	    end
    end.

join(MeetingId, EUid) when is_list(EUid) ->
    case ?MODULE:get(MeetingId) of
	{error, Reason} ->
	    {error, Reason};
	Meeting ->
	    case lists:member(EUid, Meeting#uce_meeting.roster) of
		false ->
		    Roster = Meeting#uce_meeting.roster ++ [EUid],
		    ?MODULE:update(Meeting#uce_meeting{roster=Roster});
		true ->
		    ok
	    end
    end.

leave(MeetingId, EUid) when is_list(EUid) ->
    case ?MODULE:get(MeetingId) of
	{error, Reason} ->
	    {error, Reason};
	Meeting ->
	    case lists:member(EUid, Meeting#uce_meeting.roster) of
		true ->
		    Roster = lists:subtract(Meeting#uce_meeting.roster, [EUid]),
		    ?MODULE:update(Meeting#uce_meeting{roster=Roster});
		false ->
		    {error, not_found}
	    end
    end.

roster(MeetingId) ->
    case ?MODULE:get(MeetingId) of
	{error, Reason} ->
	    {error, Reason};
	Meeting ->
	    Meeting#uce_meeting.roster
    end.
