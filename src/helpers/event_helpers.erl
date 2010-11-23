-module(event_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([sort/1, sort/2, to_json/1, to_xml/1, from_json/1, feed/1, feed/2]).

sort(Events) ->
    ?MODULE:sort(Events, asc).
sort(Events, asc) ->
    lists:sort(fun(Event1, Event2) ->
		       Event1#uce_event.datetime < Event2#uce_event.datetime
	       end,
	       Events);
sort(Events, desc) ->
    lists:sort(fun(Event1, Event2) ->
		       Event1#uce_event.datetime > Event2#uce_event.datetime
	       end,
	       Events).

to_xml(#uce_event{id=Id,
		  location=Location,
		  from=From,
		  type=Type,
		  metadata=Metadata,
		  parent=Parent,
		  datetime=Datetime}) ->
    XMLLocation = case Location of
		      [Org, Meeting] ->
			  [{org, Org}, {meeting, Meeting}];
		      [Org] ->
			  [{org, Org}];
		      [] ->
			  []
		  end,
    XMLParent = case Parent of
		    "" ->
			[];
		    _ ->
			[{parent, Parent}]
		end,
    xmerl:export_simple_element({event,
				 [{'xsi:schemaLocation',
				   ?BASE_URL ++ "/ucengine/" ++ ?UCE_SCHEMA_LOCATION},
				  {xmlns, ?UCE_XMLNS},
				  {type, Type},
				  {datetime, integer_to_list(Datetime)}] ++
				     XMLLocation ++
				     [{from, From},
				      {id, Id}] ++
				     XMLParent,
				 [{metadata, lists:map(fun({Key, Value}) ->
							       {list_to_atom(Key), [Value]}
						       end,
						       Metadata)}]}, xmerl_xml).

to_json(#uce_event{id=Id,
		   datetime=Datetime,
		   location=Location,
		   from=From,
		   type=Type,
		   parent=Parent,
		   metadata=Metadata}) ->
    JSONLocation = case Location of
		       [Org, Meeting] ->
			   [{"org", Org}, {"meeting", Meeting}];
		       [Org] ->
			   [{"org", Org}];
		       [] ->
			   []
		   end,
    JSONParent = case Parent of
		     "" ->
			 [];
		     _ ->
			 [{parent, Parent}]
		 end,
    {struct,
     [{type, Type},
      {datetime, Datetime},
      {id, Id}] ++
	 JSONLocation ++
	 [{from, From}] ++
	 JSONParent ++
	 [{metadata, {struct, Metadata}}]};

to_json(Events)
  when is_list(Events) ->
    {array, [?MODULE:to_json(Event) || Event <- Events]}.

from_json({struct, Event}) ->
    case utils:get(Event, ["id", "datetime", "from", "org", "meeting", "type", "parent", "metadata"]) of 
	{error, Reason} ->
	    {error, Reason};
	[Id, Datetime, From, Org, Meeting, Type, Parent, {struct, Metadata}] ->
	    #uce_event{id=Id,
		       datetime=Datetime,
		       from=From,
		       location=[Org, Meeting],
		       type=Type,
		       parent=Parent,
		       metadata=Metadata}
    end.

feed(Path) ->
    feed(Path, []).
feed(Path, Params) ->
    case file:read_file(Path) of
	{error, Reason} ->
	    {error, Reason};
	{ok, Data} ->
	    {array, JSONEvents} = mochijson:decode(binary_to_list(Data)),
	    Events = lists:map(fun({struct, StructEvent} = JSONEvent) ->
				       Event = ?MODULE:from_json(JSONEvent),
                       [Id, Location, From, Type] = utils:get(Params, ["id", "location", "from", "type"],
                                                                      [Event#uce_event.id,
                                                                       Event#uce_event.location,
                                                                       Event#uce_event.from,
                                                                       Event#uce_event.type]),
				       case utils:get(StructEvent, ["offset"]) of
					   [none] ->
                           [Datetime] = utils:get(Params, ["datetime"], [Event#uce_event.datetime]),
                           Event#uce_event{ id=Id
                                            , datetime=Datetime
                                            , location=Location
                                            , from=From
                                            , type=Type
                                            };
					   [Offset] ->
					       case uce_meeting:get(Location) of
						   {error, Reason} ->
						       throw([Reason, Location]);
						   Meeting ->
						       Start = Meeting#uce_meeting.start_date,
                               [Datetime] = utils:get(Params, ["datetime"], [Start + list_to_integer(Offset)]),
						       Event#uce_event{ id=Id
                                                , datetime=Datetime
                                                , location=Location
                                                , from=From
                                                , type=Type
                                                }
					       end
				       end
			       end,
			       JSONEvents),
	    [ uce_event:add(Event) || Event <- Events ],
	    ok
    end.

