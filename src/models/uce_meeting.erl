-module(uce_meeting).

-author('victor.goya@af83.com').

-export([add/1, delete/1, update/1, get/1, list/2, join/2, leave/2, roster/1, exists/1]).

-include("uce.hrl").
-include("models.hrl").

add(#uce_meeting{} = Meeting) ->
    case ?MODULE:exists(Meeting#uce_meeting.id) of
	true ->
	    {error, conflict};
	false ->
	    [Org, _] = Meeting#uce_meeting.id,
	    case uce_org:exists(Org) of
		false ->
		    {error, not_found};
		true ->
		    ?DBMOD:add(Meeting)
	    end
    end.

delete(Id) ->
    case ?MODULE:exists(Id) of
	false ->
	    {error, not_found};
	true ->
	    ?DBMOD:delete(Id)
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

list(Org, Status) ->
    case uce_org:exists(Org) of
	false ->
	    {error, not_found};
	true ->
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

exists(Id) ->
    case ?MODULE:get(Id) of
	{error, _} ->
	    false;
	_ ->
	    true
    end.

join(Id, Uid) when is_list(Uid) ->
    case uce_user:exists(Uid) of
	false ->
	    {error, not_found};
	true ->
	    case ?MODULE:get(Id) of
		{error, Reason} ->
		    {error, Reason};
		#uce_meeting{} = Meeting ->
		    case lists:member(Uid, Meeting#uce_meeting.roster) of
			false ->
			    Roster = Meeting#uce_meeting.roster ++ [Uid],
			    ?MODULE:update(Meeting#uce_meeting{roster=Roster});
			true ->
			    ok
		    end
	    end
    end.	

leave(Id, Uid) when is_list(Uid) ->
    case uce_user:exists(Uid) of
	false ->
	    {error, not_found};
	true ->
	    case ?MODULE:get(Id) of
		{error, Reason} ->
		    {error, Reason};
		#uce_meeting{} = Meeting ->
		    case lists:member(Uid, Meeting#uce_meeting.roster) of
			false ->
			    {error, not_found};
			true ->
			    Roster = lists:subtract(Meeting#uce_meeting.roster, [Uid]),
			    ?MODULE:update(Meeting#uce_meeting{roster=Roster})
		    end
	    end
    end.

roster(Id) ->
    case ?MODULE:get(Id) of
	{error, Reason} ->
	    {error, Reason};
	#uce_meeting{} = Meeting ->
	    Meeting#uce_meeting.roster
    end.
