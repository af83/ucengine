-module(solr).

-author('thierry.bomandouki@af83.com').

-export([start/1,
		 delete/1,
		 add/2,
		 to_solrdelxml/1,
		 to_solrxml/1,
		 search/6,
		 search/7]).

-include("uce.hrl").

-export([solr_search/8]).

-define(SOLR_UPDATE, "/update?commit=true").
-define(SOLR_SELECT, "/select?indent=on&wt=json&q=").
-define(SOLR_EXTRACT, "/update/extract?commit=true&literal.id=").

start(Options) ->
	case utils:get(Options, [host, token, blacklist]) of
		[Host, Token, Blacklist]
		  when is_list(Host) and is_list(Token) and is_list(Blacklist) ->
			triggers:add(#uce_trigger{location='_',
										type='_',
										action={{solr, add}, [Host, Token, Blacklist]}}),
			ok;
		_ ->
			{error, bad_parameters}
	end.

delete(#uce_event{id=Id, type=Type} = Event) ->
	Host = utils:get(utils:get(config:get(modules), solr), host),
	http:request(post,
				 {Host ++ ?SOLR_UPDATE,
				  [],
				  [],
				  ?MODULE:to_solrdelxml(Id)
				 },
				 [{timeout, ?TIMEOUT}],
				 []),
	case Type of
		"internal.file.add" ->
			{value, {"id", FileName}} = lists:keysearch("id", 1, Event#uce_event.metadata),
			http:request(post,
						 {Host ++ ?SOLR_UPDATE,
						  [],
						  [],
						  ?MODULE:to_solrdelxml(FileName)
						 },
						 [{timeout, ?TIMEOUT}],
						 []);
		_ -> nothing
	end,
	ok.

add(#uce_event{location=[Org, Meeting], type=Type} = Event, [Host, _Token, Blacklist]) ->
	case lists:member(Type, Blacklist) of
		false ->
			ResUpdate = http:request(post,
									 {Host ++ ?SOLR_UPDATE,
									  [],
									  [],
									  ?MODULE:to_solrxml(Event)
									 },
									 [{timeout, ?TIMEOUT}],
									 []),
			case ResUpdate of
				{error, Error} = ErrRet -> ErrRet;
				_ ->
					case Type of
						"internal.file.add" ->
							{value, {"id", FileName}} = lists:keysearch("id", 1, Event#uce_event.metadata),
							{ok, {_, _, FileContent}} = http:request(?BASE_URL ++ "/file/" ++ 
																		 Org ++ "/" ++ 
																		 Meeting++ "/" ++ 
																		 FileName ++ 
																		 "?_uid=solr"++"&_sid=token"),
							http:request(post,
										 {Host ++ ?SOLR_EXTRACT ++ FileName,
										  [],
										  [],
										  FileContent
										 },
										 [{timeout, ?TIMEOUT}],
										 []);
						_ ->
							ResUpdate
					end;
				true ->
					ResUpdate
			end
	end.

%% 
to_solrdelxml(IdEvent) ->
	lists:flatten(xmerl:export_simple_element({delete, [], [{id, [], [IdEvent]}]})).

%% Encode event in solrxml format which be used to add solr index
to_solrxml(#uce_event{id=IdEvent, datetime=Datetime, location=[Org, Meeting], from=From, type=Type, metadata=Metadatas}) ->
	DocElements = lists:merge([{field, [{name,"id"}], [IdEvent]},
							   {field, [{name,"timecode"}], [utils:timestamp_to_solr_date(Datetime)]},
							   {field, [{name,"type"}], [Type]},
							   {field, [{name,"org"}], [Org]},
							   {field, [{name,"meeting"}], [case Meeting of
																none -> "";
																_ -> Meeting
															end
														   ]},
							   {field, [{name,"from"}], [From]}
							  ], metadata_to_solrxml(Metadatas)),
	Doc = {doc, [], DocElements},
	Add = {add, [], [Doc]}, 
	lists:flatten(xmerl:export_simple_element(Add, xmerl_xml)).

%Doc = {xmlelement, "doc", [], lists:merge([{xmlelement, "field", [{"name","id"}], {xmlcdata, IdEvent}},
%				       {xmlelement, "field", [{"name","timecode"}], {xmlcdata, utils:timestamp_to_solr_date(Datetime)}},
%				       {xmlelement, "field", [{"name","type"}], {xmlcdata, Type}},
%				       {xmlelement, "field", [{"name","org"}], {xmlcdata, Org}},
%				       {xmlelement, "field", [{"name","meeting"}], {xmlcdata, case Meeting of
%												  none -> "";
%												  _ -> Meeting
%											      end
%										   }},
%				       {xmlelement, "field", [{"name","from"}], {xmlcdata, From}}
%				      ], metadata_to_solrxml(Metadatas))},
%SolrXMLEvent = {xmlelement, "add", [], [Doc]},
%xml:element_to_string(SolrXMLEvent).

metadata_to_solrxml([]) ->
	[];
metadata_to_solrxml([{Key, Value} | Rest]) ->
	Metadatas = metadata_to_solrxml(Rest),
	XMLValue = lists:flatten(Value),
	[{field, [{name,"metadata_" ++ Key}], [XMLValue]} | Metadatas].

search(Org, Meeting, From, Types, Time, Texts) when is_integer(Time) ->
	TimeStart = Time - ?DEFAULT_TIME_INTERVAL,
	TimeEnd = Time + ?DEFAULT_TIME_INTERVAL,
	search(Org, Meeting, From, Types, TimeStart, TimeEnd, Texts);
search(Org, Meeting, From, Types, none, Texts) ->
	search(Org, Meeting, From, Types, none, none, Texts).


search(Org, Meeting, From, Types, TimeStart, TimeEnd, Texts) ->
	SolrConf = utils:get(config:get(modules), solr),
	SolrHost = utils:get(SolrConf, host),
	solr_search(SolrHost, Org, Meeting, From, Types, TimeStart, TimeEnd, Texts).

solr_search(SolrHost, Org, Meeting, From, Types, TimeStart, TimeEnd, Texts) ->
	TypeEvent = case Types of
					none -> none;
					_ -> utils:concat_with_pattern("+", Types)
				end,
	Text = utils:concat_with_pattern("+", Texts),
	QueryStr = if
				   Org /= none , Meeting == none, From /= none, TypeEvent == none, TimeStart == none, TimeEnd == none ->
					   % Org , From, Text
					   "org:" ++ Org ++ "+from:" ++ From ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting == none, From == none, TypeEvent == none, TimeStart == none, TimeEnd == none ->
					   % Org , Text
					   "org:" ++ Org ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent == none, TimeStart == none, TimeEnd == none ->
					   % Org , Meeting, From, Text
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent == none, TimeStart == none, TimeEnd == none ->
					   % Org , Meeting, Text
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent /= none, TimeStart == none, TimeEnd == none ->
					   % Org , Meeting, From, TypeEvent Text
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent /= none, TimeStart == none, TimeEnd == none ->
					   % Org , Meeting, TypeEvent, Text
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent /= none, TimeStart /= none, TimeEnd == none ->
					   % Org , Meeting, From, TypeEvent, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent /= none, TimeStart /= none, TimeEnd == none ->
					   % Org , Meeting, TypeEvent, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent /= none, TimeStart /= none, TimeEnd /= none ->
					   % Org , Meeting, From, TypeEvent, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+type:" ++ TypeEvent ++ "+*" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent /= none, TimeStart /= none, TimeEnd /= none ->
					   % Org , Meeting, TypeEvent, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From /= none, TypeEvent /= none, TimeStart == none, TimeEnd == none ->
					   % From, TypeEvent, Text
					   "from:" ++ From ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ "";
				   
				   Org == none , Meeting == none, From == none, TypeEvent /= none, TimeStart == none, TimeEnd == none ->
					   % TypeEvent, Text
					   "type:" ++ TypeEvent ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting == none, From /= none, TypeEvent /= none, TimeStart == none, TimeEnd == none ->
					   % From, Org, TypeEvent, Text
					   "org:" ++ Org ++ "+from:" ++ From ++ "type:" ++ TypeEvent ++ "+" ++ Text ++ "";
				   
				   Org /= none , Meeting == none, From == none, TypeEvent /= none, TimeStart == none, TimeEnd == none ->
					   % Org, TypeEvent, Text
					   "org:" ++ Org ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ "";
				   
				   Org == none , Meeting == none, From /= none, TypeEvent /= none, TimeStart /= none, TimeEnd == none ->
					   % From, TypeEvent, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "from:" ++ From ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From == none, TypeEvent /= none, TimeStart /= none, TimeEnd == none ->
					   % TypeEvent, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From /= none, TypeEvent /= none, TimeStart /= none, TimeEnd == none ->
					   % TypeEvent, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "from:" ++ From ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From == none, TypeEvent /= none, TimeStart /= none, TimeEnd == none ->
					   % TypeEvent, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From /= none, TypeEvent == none, TimeStart /= none, TimeEnd == none ->
					   % From, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From == none, TypeEvent == none, TimeStart /= none, TimeEnd == none ->
					   % TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting == none, From /= none, TypeEvent == none, TimeStart /= none, TimeEnd == none ->
					   % Org, From, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "org:" ++ Org ++ "+from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting == none, From == none, TypeEvent == none, TimeStart /= none, TimeEnd == none ->
					   % Org, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "org:" ++ Org ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent == none, TimeStart /= none, TimeEnd == none ->
					   % Org, Meeting, From, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent == none, TimeStart /= none, TimeEnd == none ->
					   % Org, Meeting, TimeStart, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ Start ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent == none, TimeStart /= none, TimeEnd /= none ->
					   % Org, Meeting, From, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent == none, TimeStart /= none, TimeEnd /= none ->
					   % Org, Meeting, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From /= none, TypeEvent == none, TimeStart == none, TimeEnd /= none ->
					   % From, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From == none, TypeEvent == none, TimeStart == none, TimeEnd /= none ->
					   % TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From /= none, TypeEvent == none, TimeStart /= none, TimeEnd /= none ->
					   % From, TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org == none , Meeting == none, From == none, TypeEvent == none, TimeStart /= none, TimeEnd /= none ->
					   % TimeStart, TimeEnd, Text
					   Start = utils:timestamp_to_solr_date(TimeStart),
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ Start ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting == none, From /= none, TypeEvent == none, TimeStart == none, TimeEnd /= none ->
					   % Org, From, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting == none, From == none, TypeEvent == none, TimeStart == none, TimeEnd /= none ->
					   % Org, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent == none, TimeStart == none, TimeEnd /= none ->
					   % Org, Meeting, From, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent == none, TimeStart == none, TimeEnd /= none ->
					   % Org, Meeting, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From /= none, TypeEvent /= none, TimeStart == none, TimeEnd /= none ->
					   % Org, Meeting, From, TypeEvent, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+from:" ++ From ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   Org /= none , Meeting /= none, From == none, TypeEvent /= none, TimeStart == none, TimeEnd /= none ->
					   % Org, Meeting, TypeEvent, TimeEnd, Text
					   End = utils:timestamp_to_solr_date(TimeEnd),
					   "org:" ++ Org ++ "+meeting:" ++ Meeting ++ "+type:" ++ TypeEvent ++ "+" ++ Text ++ ""
						   ++ "&facet.date=timecode&facet.date.start=" ++ End ++ "&facet.date.end=" ++ End ++ "&facet.date.gap=%2B1HOUR";
				   
				   true ->
					   "" ++ Text ++ ""
			   end,
	Response = http:request(SolrHost ++ ?SOLR_SELECT ++ QueryStr ),
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
