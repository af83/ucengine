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
		{ok, nothing} ->
		    nothing;
		{ok, Result} ->
		    io:format("Success: ~p~n", [Result]);
		{error, Reason} ->
		    io:format("Error: ~p~n", [Reason]);
		Exception when is_list(Exception) ->
		    io:format("Fatal: " ++ Exception ++ "~n");
		Exception ->
		    io:format("Fatal: ~p~n", [Exception])
	    end;
	A ->
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

format_field([], []) ->
    [];
format_field([Metadata|Values], [metadata|Fields]) ->
    FormattedMetadata =
	[io_lib:format("metadata[~p]: ~ts", [Key, Value]) || {Key, Value} <- Metadata],
    string:join(FormattedMetadata, "~n") ++ format_field(Values, Fields);
format_field([Value|Values], [Field|Fields]) when is_list(Value) ->
    io_lib:format("~p: ~ts~n", [Field, unicode:characters_to_binary(Value)]) ++ format_field(Values, Fields);
format_field([Value|Values], [Field|Fields]) ->
    io_lib:format("~p: ~p~n", [Field, Value]) ++ format_field(Values, Fields).

format(Record, Fields) ->
    [_|Values] = tuple_to_list(Record),
    format_field(Values, Fields).
	    
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

%%
%% Org
%%
action(org, add, Args) ->
    {[[Name]], Metadata} = getopt(["name"], Args),
    call(org, add, [#uce_org{name=Name, metadata=Metadata}]);

action(org, get, Args) ->
    {[[Name]], _} = getopt(["name"], Args),
    Response = call(org, get, [Name]),
    format(Response, record_info(fields, uce_org));

action(org, update, Args) ->
    {[[Name]], Metadata} = getopt(["name"], Args),
    call(org, update, [#uce_org{name=Name, metadata=Metadata}]);

action(org, delete, Args) ->
    {[[Name]], _} = getopt(["name"], Args),
    call(org, update, [Name]);

%%
%% Meeting
%%
action(meeting, add, Args) ->
    {[[Name], [Org], Start, End], Metadata} =
	getopt(["name", "org", "start", "end"], Args),
    call(meeting, add, [#uce_meeting{id=[Org, Name],
				     start_date=parse_date(Start),
				     end_date=parse_date(End),
				     metadata=Metadata}]);

action(meeting, delete, Args) ->
    {[[Name], [Org]], _} = getopt(["name", "org"], Args),
    call(meeting, delete, [{Org, Name}]);

action(meeting, get, Args) ->
    {[[Name], [Org]], _} = getopt(["name", "org"], Args),
    call(meeting, get, [{Org, Name}]);

action(meeting, update, Args) ->
    {[[Name], [Org], Start, End], Metadata} =
	getopt(["name", "org", "start", "end"], Args),
    call(meeting, update, [#uce_meeting{id=[Org, Name],
					start_date=parse_date(Start),
					end_date=parse_date(End),
					metadata=Metadata}]);

action(meeting, list, Args) ->
    case getopt([name, status], Args) of
	{[[Name], [Status]], _} ->
	    call(meeting, list, [Name, Status]);
	{[[Name]], _} ->
	    call(meeting, list, [Name, "all"])
    end;

%%
%% Utils
%%
action(demo, start, Args) ->
    NodeStr = "ucengine@localhost",
    rpc:call(list_to_atom(NodeStr), demo, start, Args);

action(Object, _, _) ->
    usage(Object).
