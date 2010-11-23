-module(uce_event_solr_search).

-author('thierry.bomandouki@af83.com').

-export([add/1, list/7]).

-include("uce.hrl").

-define(DEFAULT_HOST, "http://localhost:8983/solr").
-define(SOLR_UPDATE, "/update?commit=true").
-define(SOLR_SELECT, "/select?").

add(Event) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    http:request(post,
		 {Host ++ ?SOLR_UPDATE,
		  [],
		  [],
		  to_solrxml(Event)
		 },
		 [{timeout, ?TIMEOUT}],
		 []).


%% Encode event in solrxml format which be used to add solr index
to_solrxml(#uce_event{id=Id,
		      datetime=Datetime,
		      location=Location,
		      from=From,
		      type=Type,
		      metadata=Metadata}) ->
    LocationElement =
	case Location of
	    ['_', '_'] ->
		[];
	    [Org, '_'] ->
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
		   {field, [{name,"datetime"}], [timestamp_to_date(Datetime)]},
		   {field, [{name,"type"}], [Type]},
		   {field, [{name,"from"}], [From]}] ++
	LocationElement ++ MetadataFlattenElement ++ MetadataElement,

    Add = {add, [], [{doc, [], DocElements}]},
    lists:flatten(xmerl:export_simple_element(Add, xmerl_xml)).

metadata_to_solrxml([]) ->
    [];
metadata_to_solrxml([{Key, Value} | Rest]) ->
    Metadatas = metadata_to_solrxml(Rest),
    XMLValue = lists:flatten(Value),
    [{field, [{name,"metadata_" ++ Key}], [XMLValue]} | Metadatas].

int2str(Int) when Int < 10 ->
	"0" ++ integer_to_list(Int);
int2str(Int) ->
	integer_to_list(Int).

timestamp_to_date(SDate) when is_list(SDate) ->
    timestamp_to_date(list_to_integer(SDate));
timestamp_to_date(Date) when is_integer(Date) ->
    if
	Date > 0 ->
	    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
	    {{Y,M,D},{H,Min,S}} = calendar:gregorian_seconds_to_datetime(BaseDate + (Date div 1000) ),
	    int2str(Y) ++ "-" ++ int2str(M) ++ "-" ++ int2str(D) ++ "T" ++ int2str(H) ++ ":" ++ int2str(Min) ++ ":" ++ int2str(S) ++ "Z";
	true ->
	    none
    end.

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

search(Host, [Org, Meeting], Search, From, Type, Start, End, Parent) ->
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
		[{"facet.date", "timecode"},
		 {"facet.date.gap", "+1HOUR"},
		 {"facet.date.start", timestamp_to_date(Start)},
		 {"facet.date.end", timestamp_to_date(End)}];
	    Start /= 0 ->
		[{"facet.date", "timecode"},
		 {"facet.date.gap", "+1HOUR"},
		 {"facet.date.start", timestamp_to_date(Start)}];
	    End /= infinity ->
		[{"facet.date", "timecode"},
		 {"facet.date.gap", "+1HOUR"},
		 {"facet.date.end", timestamp_to_date(End)}];
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

    io:format("R: ~p~n", [Host ++ ?SOLR_SELECT ++ string:join(EncodedParams, "&")]),

    Response = http:request(Host ++ ?SOLR_SELECT ++ string:join(EncodedParams, "&")),
    Result = case Response of
		 {ok, {_, _, JSONStr}} -> JSON = mochijson:decode(JSONStr),
					  {struct, ArrayJSON} = JSON,
					  {value, {"response", {struct, RespArrayJSON}}} = lists:keysearch("response", 1, ArrayJSON),
					  {value, {"docs", {array, ArrayResults}}} = lists:keysearch("docs", 1, RespArrayJSON),
					  make_list_json_events(ArrayResults);
		 {error, Error} = RetErr -> RetErr;  
		 _ -> []
	     end,
    Result.

make_list_json_events([]) -> [];
make_list_json_events([HdJSON | TlJSON]) ->
    {struct, StructInfos} = HdJSON,
    {value, {"id", IdEvent}} = lists:keysearch("id", 1, StructInfos),
    Event = event:get(IdEvent),
    [Event] ++ make_list_json_events(TlJSON).
