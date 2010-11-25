-module(meeting_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([to_json/1, to_ical/1]).

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
