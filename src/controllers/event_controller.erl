-module(event_controller).

-export([init/0, get/3, list/3, add/3]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

init() ->
    [#uce_route{module="Events",
		method='GET',
		regexp="/event/([^/]+)/([^/]+)/?",
		types=[any, event],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, get,
			    ["uid"],
			    [required],
			    [string],
			    [user]}]},

     #uce_route{module="Events",
		method='GET',
		regexp="/event/?([^/]+)?/?",
		types=[meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, list,
			    ["uid",
			     "search",
			     "type",
			     "from",
			     "start",
			     "end",
			     "count",
			     "page",
			     "order",
			     "parent",
			     "_async"],
			    [required, '_', '_', '_', 0, infinity, infinity, 1, asc, '_', "no"],
			    [string,
			     [string, atom],
			     [string, atom],
			     [string, atom],
			     integer,
			     [integer, atom],
			     [integer, atom],
			     integer,
			     atom,
			     [string, atom],
			     string],
			    [user, any, any, user, any, any, any, any, any, event, any]}]},
     
     #uce_route{module="Events",
		method='PUT',
		regexp="/event/?([^/]+)?/?",
		types=[meeting],
		callbacks=[{presence_controller, check,
			    ["uid", "sid"],
			    [required, required],
			    [string, string],
			    [user, presence]},
			   {?MODULE, add,
			    ["uid", "type", "to", "parent", "metadata"],
			    [required, required, "all", "", []],
			    [string, string, string, string, dictionary],
			    [user, any, user, event, any]}]}].

get([_, Id], [Uid], _) ->
    case uce_acl:check(Uid, "event", "get", [""], [{"id", Id}]) of
        {ok, false} ->
            {error, unauthorized};
        {ok, true} ->
            case uce_event:get(Id) of
                {error, Reason} ->
                    {error, Reason};
                {ok, #uce_event{to=To} = Event} ->
                    if
                        To == "all" ->
                            json_helpers:json(event_helpers:to_json(Event));
                        To == Uid ->
                            json_helpers:json(event_helpers:to_json(Event));
                        true ->
                            {error, unauthorized}
                    end
            end
    end.

list([], Match, Arg) ->
    ?MODULE:list([""], Match, Arg);
list(Location, [Uid, Search, Type, From, Start, End, Count, Page, Order, Parent, Async], Arg) ->
    case uce_acl:check(Uid, "event", "list", Location, [{"from", From}]) of
	{ok, true} ->
	    Types = case Type of
			'_' ->
			    ['_'];
			_ ->
			    string:tokens(Type, ",")
		    end,
	    Keywords = case Search of
			   '_' ->
			       '_';
			   _ ->
			       string:tokens(Search, " ")
		       end,
	    case uce_event:list(Location, Keywords, From, Types, Uid, Start, End, Parent) of
		{error, Reason} ->
		    {error, Reason};
		{ok, []} ->
		    case Async of
			"no" ->
			    json_helpers:json(event_helpers:to_json([]));
			"lp" ->
			    uce_async_lp:wait(Location,
					      Keywords,
					      From,
					      Types,
					      Uid,
					      Start,
					      End,
					      Parent,
					      Arg#arg.clisock);
			"ws" ->
			    uce_async_ws:wait(Location,
					      Uid,
					      Keywords,
					      Type,
					      From,
					      Start,
					      Arg#arg.clisock);
			_ ->
			    {error, bad_parameters}
		    end;
		{ok, Events} ->
		    case helpers:paginate(event_helpers:sort(Events), Count, Page, Order) of
			{error, Reason} ->
			    {error, Reason};
			EventPage ->
			    json_helpers:json(event_helpers:to_json(EventPage))
		    end
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.

add([], [Uid, Type, To, Parent, Metadata], Arg) ->
    add([""], [Uid, Type, To, Parent, Metadata], Arg);
add(Location, [Uid, Type, To, Parent, Metadata], _) ->
    case uce_acl:check(Uid, "event", "add", Location, [{"type", Type},
                                                       {"to", To}]) of
	{ok, true} ->
	    case uce_event:add(#uce_event{location=Location,
					  from=Uid,
					  type=Type,
					  to=To,
					  parent=Parent,
					  metadata=Metadata}) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Id} ->
		    json_helpers:created(Id)
	    end;
	{ok, false} ->
	    {error, unauthorized}
    end.
