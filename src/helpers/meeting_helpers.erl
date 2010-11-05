-module(meeting_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([to_json/1, to_ical/1, exists/1]).

to_json(#uce_meeting{id=[Org, Meeting],
		       start_date=StartDate,
		       end_date=EndDate,
		       roster=Roster,
		       metadata=Metadata}) ->
    {struct, [{org, Org},
	      {name, Meeting},
	      {start_date, StartDate},
	      {end_date, case EndDate of
			     ?NEVER_ENDING_MEETING ->
				 "never";
			     _ ->
				 integer_to_list(EndDate)
			 end},
	      {roster, {array, Roster}},
	      {metadata, {struct, Metadata}}]}.

to_ical(#uce_meeting{start_date=Start, end_date=End, metadata=Metadata}) ->
    Desc = case lists:keysearch("description", 1, Metadata) of
	       {value, {"description", Value}} ->
		   Value;
	       _ ->
		   []
	   end,
    "BEGIN:VCALENDAR
    VERSION:2.0
    PRODID:-//hacksw/handcal//NONSGML v1.0//EN
    BEGIN:VEVENT
    DTSTART:" ++ utils:timestamp_to_ical_date(Start) ++ "
    DTEND:" ++ utils:timestamp_to_ical_date(End) ++ "
    SUMMARY:" ++ Desc ++ "
    END:VEVENT
    END:VCALENDAR".

exists(Location) ->
    case Location of
	[Org, Meeting] ->
	    case uce_meeting:get([Org, Meeting]) of
		{error, _} ->
		    false;
		_ ->
		    true
	    end;
	[Org] ->
	    case uce_org:get(Org) of
		{error, _} ->
		    false;
		_ ->
		    true
	    end;
	[] ->
	    true
    end.
