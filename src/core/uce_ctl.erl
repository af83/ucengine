-module(uce_ctl).

-author('victor.goya@af83.com').

-export([start/0, stop/0, getopt/2]).

-include("uce.hrl").

args_to_dictionary([]) ->
    [];
args_to_dictionary([{Key, Value}|Tail]) when is_atom(Key) ->
    args_to_dictionary([{atom_to_list(Key), Value}] ++ Tail);
args_to_dictionary([{[$- | Key], Value} | Tail]) ->
    [{Key, Value}] ++ args_to_dictionary(Tail);
args_to_dictionary([_|Tail]) ->
    [] ++ args_to_dictionary(Tail).

start() ->
    Command = init:get_arguments(),
    case utils:get(Command, [object, action]) of
	[['-help'], _] ->
	    usage();
	[[Object], []] ->
	    usage(list_to_atom(Object));
	[[Object], [Action]] ->
	    case catch action(list_to_atom(Object),
			      list_to_atom(Action),
			      args_to_dictionary(Command)) of
		ok ->
		    io:format("~n"),
		    init:stop(0);
		error ->
		    io:format("~n"),
		    init:stop(2);
		Exception when is_list(Exception) ->
		    io:format("Fatal: " ++ Exception ++ "~n"),
		    init:stop(2);
		Exception ->
		    io:format("Fatal: ~p~n", [Exception]),
		    init:stop(2)
	    end;
	_ ->
	    usage()
    end,
    halt().

stop() ->
    ok.

usage() ->
    usage(none).
usage(Object) ->
    io:format("Usage:~n"),
    io:format("ucectl <object> <action> [--<parameter> <value>]~n~n"),

    if
	Object == none ; Object == org ->
	    io:format("Organisations:~n"),
	    io:format("\torg add --name <name> [--<metadata key> <metadata value>]~n"),
	    io:format("\torg update --name <name> [--<metadata key> <metadata value>]~n"),
	    io:format("\torg get --name <name>~n"),
	    io:format("\torg delete --name <name>~n"),
	    io:format("\torg list~n~n");
	true ->
	    nothing
    end,
    if
	Object == none ; Object == meeting ->
	    io:format("Meetings:~n"),
	    io:format("\tmeeting add --org <org> --name <name> --start <date> --end <date> [--<metadata key> <metadata value>]~n"),
	    io:format("\tmeeting update --org <org> --name <name> --start <date> --end <date> [--<metadata key> <metadata value>]~n"),
	    io:format("\tmeeting get --org <org> --name <name>~n"),
	    io:format("\tmeeting delete --org <org> --name <name>~n"),
	    io:format("\tmeeting list --org <org> [--status <status>]~n~n");
	true ->
	    nothing
    end,
    io:format("Formatting:~n"),
    io:format("\t<date>: ISO8601 formatted date (ex. '2010-25-12 00:00:01')~n~n"),
    io:format("UCengine (c) AF83 - http://ucengine.org~n"),
    {ok, nothing}.

getvalues([], _) ->
    [];
getvalues([Key|Keys], Args) ->
    case lists:keyfind(Key, 1, Args) of
	{_, ArgValue} ->
	    [ArgValue];
	false ->
	    [none]
    end ++ getvalues(Keys, Args).

getopt(Keys, Args) ->
    Values = getvalues(Keys, Args),
    RawRemaining = lists:filter(fun({ArgKey, _}) ->
					lists:member(ArgKey, Keys) == false
				end,
				Args),
    Remaining = [{Key, string:join(Value, " ")} || {Key, Value} <- RawRemaining],
    {Values, Remaining}.

json_escape(String) when is_list(String) ->
    re:replace(String, "\"", "\\\"", [{return, list}]);
json_escape(Integer) when is_integer(Integer) ->
    Integer.

display_metadata(json, []) ->
    [];
display_metadata(json, [{Key, Value}|Tail]) ->
    io:format("        ~p: ~p", [json_escape(Key), json_escape(Value)]),
    case Tail of
	[] ->
	    io:format("~n");
	_ ->
	    io:format(",~n")
    end,
   display_metadata(json, Tail).

display_field(json, [], []) ->
    [];
display_field(json, [Value|Values], [Field|Fields]) ->
    case Field of
	metadata ->
	    io:format("    \"metadata\": {~n"),
	    display_metadata(json, Value),
	    io:format("    }");
	_ ->
	    io:format("    ~p: ~p", [json_escape(atom_to_list(Field)),
					     json_escape(Value)])
    end,
    case Fields of
	[] ->
	    io:format("~n");
	_ ->
	    io:format(",~n")
    end,
    display_field(json, Values, Fields).

display(json, Records, Fields) when is_list(Records) ->
    io:format("["),
    [display(json, Record, Fields) || Record <- Records],
    io:format("]");
display(json, Record, Fields) ->
    [_|Values] = tuple_to_list(Record),
    io:format("{~n"),
    display_field(json, Values, Fields),
    io:format("}");

display(erlang, Record, _) ->
    io:format("~p~n", [Record]).    
    
call(Object, Action, Args) ->
    Module = list_to_atom("uce_" ++ atom_to_list(Object)),
    NodeStr = "ucengine@localhost",
    rpc:call(list_to_atom(NodeStr), Module, Action, Args).

parse_date([Date, Time]) when is_list(Date) , is_list(Time) ->
    case string:tokens(Date ++ " " ++ Time, "- :") of
	[Year, Month, Day, Hours, Minutes, Seconds] ->
	    DateTime = {{list_to_integer(Year),
			 list_to_integer(Month),
			 list_to_integer(Day)},
			{list_to_integer(Hours),
			 list_to_integer(Minutes),
			 list_to_integer(Seconds)}},
	    Epoch =
		calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
	    (calendar:datetime_to_gregorian_seconds(DateTime) - Epoch) * 1000;
	_ ->
	    {error, bad_date}
    end.

success(Result) ->
    io:format("Success: ~p", [Result]),
    ok.

error(Reason) ->
    io:format("Error: ~p", [Reason]),
    error.

%%
%% Org
%%
action(org, add, Args) ->
    {[[Name]], Metadata} = getopt(["name"], Args),
    case call(org, add, [#uce_org{name=Name, metadata=Metadata}]) of
	{ok, created} ->
	    success(created);
	{error, Reason} ->
	    error(Reason)
    end;

action(org, get, Args) ->
    {[[Name]], _} = getopt(["name"], Args),
    case call(org, get, [Name]) of
	{ok, Record} ->
	    display(json, Record, record_info(fields, uce_org));
	{error, Reason} ->
	    error(Reason)
    end;

action(org, update, Args) ->
    {[[Name]], Metadata} = getopt(["name"], Args),
    case call(org, update, [#uce_org{name=Name, metadata=Metadata}]) of
	{ok, updated} ->
	    success(updated);
	{error, Reason} ->
	    error(Reason)
    end;

action(org, delete, Args) ->
    {[[Name]], _} = getopt(["name"], Args),
    case call(org, delete, [Name]) of
	{ok, deleted} ->
	    success(deleted);
	{error, Reason} ->
	   error(Reason)
    end;

action(org, list, _Args) ->
    case call(org, list, []) of
	{ok, Records} ->
	    display(json, Records, record_info(fields, uce_org));
	{error, Reason} ->
	    error(Reason)
    end;

%%
%% Meeting
%%
action(meeting, add, Args) ->
    {[[Name], [Org], Start, End], Metadata} =
	getopt(["name", "org", "start", "end"], Args),
    case call(meeting, add, [#uce_meeting{id=[Org, Name],
					  start_date=parse_date(Start),
					  end_date=parse_date(End),
					  metadata=Metadata}]) of
	{ok, created} ->
	    success(created);
	{error, Reason} ->
	    error(Reason)
    end;

action(meeting, delete, Args) ->
    {[[Name], [Org]], _} = getopt(["name", "org"], Args),
    case call(meeting, delete, [[Org, Name]]) of
	{ok, deleted} ->
	    success(deleted);
	{error, Reason} ->
	    error(Reason)
    end;

action(meeting, get, Args) ->
    {[[Name], [Org]], _} = getopt(["name", "org"], Args),
    case call(meeting, get, [[Org, Name]]) of
	{ok, Record} ->
	    display(json, Record, record_info(fields, uce_meeting));
	{error, Reason} ->
	    error(Reason)
	end;

action(meeting, update, Args) ->
    {[[Name], [Org], Start, End], Metadata} =
	getopt(["name", "org", "start", "end"], Args),
    case call(meeting, update, [#uce_meeting{id=[Org, Name],
					     start_date=parse_date(Start),
					     end_date=parse_date(End),
					     metadata=Metadata}]) of
	{ok, updated} ->
	    success(updated);
	{error, Reason} ->
	    error(Reason)
    end;

action(meeting, list, Args) ->
    Res = case getopt(["org", "status"], Args) of
	      {[[Org], [Status]], _} ->
		  call(meeting, list, [Org, Status]);
	      {[[Org], none], _} ->
		  call(meeting, list, [Org, "all"])
	  end,
    case Res of
	{ok, Records} ->
	    display(json, Records, record_info(fields, uce_meeting));
	{error, Reason} ->
	    error(Reason)
    end;


%%
%% Utils
%%
action(demo, start, Args) ->
    NodeStr = "ucengine@localhost",
    rpc:call(list_to_atom(NodeStr), demo, start, Args);

action(Object, _, _) ->
    usage(Object).
