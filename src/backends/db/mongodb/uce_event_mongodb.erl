-module(uce_event_mongodb).

-author('victor.goya@af83.com').

-export([
	 add/1,
	 get/1,
	 list/6,
	 from_collection/1,
	 to_collection/1
	 ]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_event{} = Event) ->
    emongo:insert(?MONGO_POOL, ?MODULE:to_collection(Event)),
    ok.

get(Id) ->
    case emongo:find_one(?MONGO_POOL, "uce_event", [{"id", Id}]) of
	[Collection] ->
	    ?MODULE:from_collection(Collection);
	_ ->
	    {error, not_found}
    end.

list(Location, Search, From, Type, Start, End) ->
    SelectLocation = case Location of
			 ['_', '_'] ->
			     [];
			 [Org, '_'] ->
			     [{"org", Org}];
			 [Org, Meeting] ->
			     [{"org", Org}, {"meeting", Meeting}]
		     end,
    SelectFrom = if
		       From  == '_' ->
			   [];
		       true ->
			   [{"from", From}]
		   end,
    SelectType = if
		       Type == '_' ->
			   [];
		       true ->
			   [{"type", Type}]
		   end,
    SelectTime = if
		       Start == 0, End == infinity -> 
			   [];
		       Start /= 0, End == infinity ->
			   [{"datetime", [{'>', Start}]}];
		       Start /= 0, End /= infinity ->
			   [{"datetime", [{'>', Start},
					  {'<', End}]}];
		       Start == 0, End /= infinity ->
			   [{"datetime", [{'<', End}]}];
		       true ->
			   []
	       end,
    Events = lists:map(fun(Collection) ->
			       ?MODULE:from_collection(Collection)
		       end,
		       emongo:find_all(?MONGO_POOL,"uce_event",
				       SelectLocation ++
					   SelectFrom ++
					   SelectType ++
					   SelectTime,
				       [{orderby, [{"this.datetime", asc}]}])),
    case Search of
	'_' ->
	    Events;
	_ ->
	    event_helpers:search(Events, Search)
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
		  ["id", "org", "meeting", "from", "metadata", "datetime", "type"]) of
	[Id, Org, Meeting, From, Metadata, Datetime, Type] ->
	    Location = case [Org, Meeting] of
			   ["_", "_"] ->
			       [];
			   [_, "_"] ->
			       [Org];
			   [_, _] ->
			       [Org, Meeting]
		       end,
	    #uce_event{id=Id,
			 datetime=Datetime,
			 from=From,
			 location=Location,
			 type=Type,
			 metadata=Metadata};
	_ ->
	    {error, bad_parameters}
    end.

to_collection(#uce_event{id=Id,
			   location=Location,
			   from=From,
			   metadata=Metadata,
			   datetime=Datetime,
			   type=Type}) ->
    [Org, Meeting] = case Location of
			 [] ->
			     ["_", "_"];
			 [LocOrg] ->
			     [LocOrg, "_"];
			 [LocOrg, LocMeeting] ->
			     [LocOrg, LocMeeting]
		     end,
    #collection{name="uce_event", 
		fields=[{"id", Id},
			{"org", Org},
			{"meeting", Meeting},
			{"from", From},
			{"metadata", Metadata},
			{"datetime", Datetime},
			{"type", Type}
		       ],
		index=[{"id", Id},
		       {"org", Org},
		       {"meeting", Meeting},
		       {"from", From},
		       {"datetime", Datetime},
		       {"type", Type}]}.
