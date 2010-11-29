-module(uce_event_solr_search).

-author('thierry.bomandouki@af83.com').

-export([add/1, list/7, delete/1]).

-include("uce.hrl").

-define(DEFAULT_HOST, "http://localhost:8983/solr").
-define(SOLR_UPDATE, "/update?commit=true").
-define(SOLR_SELECT, "/select?").

-define(META_PREFIX, "metadata_").

add(Event) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    httpc:request(post,
		 {Host ++ ?SOLR_UPDATE,
		  [],
		  [],
		  to_solrxml(Event)
		 },
		 [{timeout, ?TIMEOUT}],
		 []),
    ok.

%% Encode event in solrxml format which be used to add solr index
to_solrxml(#uce_event{id=Id,
		      datetime=Datetime,
		      location=Location,
		      from=From,
		      type=Type,
		      metadata=Metadata}) ->
    LocationElement =
	case Location of
	    ["", ""] ->
		[];
	    [Org, ""] ->
		[{field, [{name,"org"}], [Org]}];
	    [Org, Meeting] ->
		[{field, [{name,"org"}], [Org]},
		 {field, [{name,"meeting"}], [Meeting]}]
	end,

    MetadataFlattenElement =
	[{field, [{name, "metadata"}], [lists:flatten([Value || {_, Value} <- Metadata])]}],

    MetadataElement =
	lists:map(fun({Key, Value}) ->
			  {field, [{name, "metadata_" ++ Key}], [Value]}
		  end,
		  Metadata),

    DocElements = [{field, [{name,"id"}], [Id]},
		   {field, [{name,"datetime"}], [integer_to_list(Datetime)]},
		   {field, [{name,"type"}], [Type]},
		   {field, [{name,"from"}], [From]}] ++
	LocationElement ++
	MetadataFlattenElement ++
	MetadataElement,

    Add = {add, [], [{doc, [], DocElements}]},
    lists:flatten(xmerl:export_simple_element(Add, xmerl_xml)).

params_to_query([{Key, Value}|Tail]) ->
    case Key of
	[] ->
	    [];
	_ ->
	    Key ++ ":"
    end ++ Value ++ case Tail of
			[] ->
			    [];
			_ ->
			    " +" ++ params_to_query(Tail)
		    end.

list(Location, Search, From, Type, Start, End, Parent) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    search(Host, Location, Search, From, Type, Start, End, Parent).

search(Host, [Org, Meeting], Search, From, Type, Start, End, _) ->
    OrgSelector =
	if
	    Org /= '_' ->
		[{"org", Org}];
	    true ->
		[]
	end,

    MeetingSelector =
	if
	    Meeting /= '_' ->
		[{"meeting", Meeting}];
	    true ->
		[]
	end,

    SearchSelector =
	if
	    Search /= '_' ->
		[{"metadata", string:join([Key ++ "*" || Key <- Search], "+")}];
	    true ->
		[]
	end,

    FromSelector =
	if
	    From /= '_' ->
		[{"from", From}];
	    true ->
		[]
	end,

    TypeSelector =
	if
	    Type /= '_' ->
		[{"type", Type}];
	    true ->
		[]
	end,

    TimeSelector =
	if
	    Start /= 0, End /= infinity ->
		[{"date", "[" ++ integer_to_list(Start) ++ " TO " ++ integer_to_list(End) ++ "]"}];
	    Start /= 0 ->
		[{"date", "[" ++ integer_to_list(Start) ++ " TO *"}];
	    End /= infinity ->
		[{"date", "* TO " ++ integer_to_list(End) ++ "]"}];
	    true ->
		[]
	end,

    Query = [{"q", params_to_query(OrgSelector ++
				       MeetingSelector ++
				       FromSelector ++
				       TypeSelector ++
				       SearchSelector)}],

    EncodedParams = [yaws_api:url_encode(Elem) ++ "=" ++ yaws_api:url_encode(Value) ||
			{Elem, Value} <- Query ++ TimeSelector ++ [{"wt", "json"}]],

    case httpc:request(Host ++ ?SOLR_SELECT ++ string:join(EncodedParams, "&")) of
	{error, _} ->
	    {error, bad_parameters};	   
	{ok, {_, _, JSON}} ->
	    json_to_events(mochijson:decode(JSON));
	_ ->
	    []
    end.

json_to_events({struct, JSON}) ->
    {"response", {struct, ResponseJSON}} = lists:keyfind("response", 1, JSON),
    {"docs", {array, DocsJSON}} = lists:keyfind("docs", 1, ResponseJSON),
    make_list_json_events(DocsJSON).

make_list_json_events([]) ->
    [];
make_list_json_events([{struct, Elems}|Tail]) ->
    case utils:get(Elems,
		   ["id",
		    "datetime",
		    "org",
		    "meeting",
		    "from",
		    "to",
		    "type",
		    "parent"],
		   [none,
		    none,
		    {array, [""]},
		    {array, [""]},
		    none,
		    {array, ["all"]},
		    none,
		    {array, [""]}]) of

	[none, _, _, _, _, _, _, _] ->
	    {error, bad_record};
	[_, none, _, _, _, _, _, _] ->
	    {error, bad_record};
	[_, _, _, _, none, _, _, _] ->
	    {error, bad_record};
	[_, _, _, _, _, _, none, _] ->
	    {error, bad_record};

	[{array, [Id]},
	 {array, [Datetime]},
	 {array, [Org]},
	 {array, [Meeting]},
	 {array, [From]},
	 {array, [To]},
	 {array, [Type]},
	 {array, [Parent]}] ->
	    FlatMetadata =
		lists:filter(fun({Name, _}) ->
				     if
					 length(Name) < length(?META_PREFIX) ->
					     false;
					 true ->
					     SubName = string:substr(Name, 1, length(?META_PREFIX)),
					     if
						 SubName == ?META_PREFIX ->
						     true;
						 true ->
						     false
					     end
				     end
			     end,
			     Elems),
	    Metadata = lists:map(fun({Name, {array, [Value]}}) ->
					 {string:substr(Name, length(?META_PREFIX) + 1, length(Name)),
					  Value}
				 end,
				 FlatMetadata),
	    [#uce_event{id=Id,
			datetime=list_to_integer(Datetime),
			location=[Org, Meeting],
			from=From,
			to=To,
			type=Type,
			parent=Parent,
			metadata=Metadata}] ++ make_list_json_events(Tail)
    end.

delete(Id) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    httpc:request(post,
                  {Host ++ ?SOLR_UPDATE,
                   [],
                   [],
                   "<delete><query>"++ Id ++"</query></delete>"
                  },
                  [{timeout, ?TIMEOUT}],
                  []),
    ok.