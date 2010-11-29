-module(uce_meeting_mongodb).

-author('victor.goya@af83.com').

-export([add/1,
	 delete/1,
	 get/1,
	 update/1,
	 list/1,
	 to_collection/1,
	 from_collection/1]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_meeting{} = Meeting) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_meeting", ?MODULE:to_collection(Meeting)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

delete([Org, Meeting]) ->
    case emongo:delete(?MONGO_POOL, "uce_meeting", [{"org", Org},
						    {"meeting", Meeting}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

get([Org, Meeting]) ->
    case catch emongo:find_one(?MONGO_POOL, "uce_meeting",
                               [{"org", Org}, {"meeting", Meeting}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	[Record] ->
	    ?MODULE:from_collection(Record);
	_ ->
	    {error, not_found}
    end.

update(#uce_meeting{id=[OrgName, MeetingName]} = Meeting) ->
    case catch emongo:update(?MONGO_POOL, "uce_meeting",
			     [{"org", OrgName}, {"meeting", MeetingName}],
			     ?MODULE:to_collection(Meeting)) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	_ ->
	    ok
    end.

list(Org) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_meeting", [{"org", Org}]) of
	{'EXIT', _} ->
	    {error, bad_parameters};
	Collections ->
	    lists:map(fun(Collection) ->
			      ?MODULE:from_collection(Collection)
		      end,
		      Collections)
    end.


to_collection(#uce_meeting{id=[OrgName, MeetingName],
			     start_date=Start,
			     end_date=End,
			     roster=Roster,
			     metadata=Metadata}) ->
    [{"org", OrgName},
     {"meeting", MeetingName},
     {"start_date", integer_to_list(Start)},
     {"end_date", integer_to_list(End)},
     {"roster", Roster},
     {"metadata", Metadata}].

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		   ["org", "meeting", "start_date", "end_date", "roster", "metadata"]) of
	[Org, Meeting, Start, End, Roster, Metadata] ->
	    #uce_meeting{id=[Org, Meeting],
			   start_date=list_to_integer(Start),
			   end_date=list_to_integer(End),
			   roster=Roster,
			   metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.
