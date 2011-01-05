-module(uce_ctl).

-author('victor.goya@af83.com').

-export([start/0, stop/0, getopt/2, action/3, success/1, error/1]).
-export([parse_date/1, timestamp_to_iso/1]).

-include("uce.hrl").

-define(DEFAULT_NODE, list_to_atom("ucengine@" ++ net_adm:localhost())).

default_node() ->
    NodeDomain =
	case re:run(atom_to_list(node()), "@(.*)", [{capture, all, list}]) of
	    {match, [_, Domain]} ->
		Domain;
	    _ ->
		"localhost"
	end,
    list_to_atom("ucengine@" ++ NodeDomain).

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
		{ok, nothing} ->
		    init:stop(0);
		{'EXIT', {{case_clause, _}, _}} ->
		    usage(list_to_atom(Object));
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
	    io:format("\torg add --name <name> [--<metadata> <value>]~n"),
	    io:format("\torg update --name <name> [--<metadata> <value>]~n"),
	    io:format("\torg get --name <name>~n"),
	    io:format("\torg delete --name <name>~n"),
	    io:format("\torg list~n~n");
	true ->
	    nothing
    end,
    if
	Object == none ; Object == meeting ->
	    io:format("Meetings:~n"),
	    io:format("\tmeeting add --org <org> --name <name> --start <date> --end <date> [--<metadata> <value>]~n"),
	    io:format("\tmeeting update --org <org> --name <name> --start <date> --end <date> [--<metadata> <value>]~n"),
	    io:format("\tmeeting get --org <org> --name <name>~n"),
	    io:format("\tmeeting delete --org <org> --name <name>~n"),
	    io:format("\tmeeting list --org <org> [--status <status>]~n~n");
	true ->
	    nothing
    end,
    if
	Object == none ; Object == user ->
	    io:format("Users:~n"),
	    io:format("\tuser add --uid <uid> --auth <auth> --credential <credential> [--<metadata> <value>]~n"),
	    io:format("\tuser update --uid <uid> --auth <auth> --credential <credential> [--<metadata> <value>]~n"),
	    io:format("\tuser get --uid <uid>~n"),
	    io:format("\tuser delete --uid <uid>~n"),
	    io:format("\tuser list~n~n");
	true ->
	    nothing
    end,
    if
	Object == none ; Object == acl ->
	    io:format("ACL:~n"),
	    io:format("\tacl add --uid <uid> --object <object> --action <action> [--org <org> --meeting <meeting>] [--condition <value>]~n"),
	    io:format("\tacl delete --uid <uid> --object <object> --action <action> [--org <org> --meeting <meeting>] [--condition <value>]~n"),
	    io:format("\tacl check --uid <uid> --object <object> --action <action> [--org <org> --meeting <meeting>] [--condition <value>]~n~n");
	true ->
	    nothing
    end,
    io:format("Formatting:~n"),
    io:format("\t<date>: ISO8601 formatted date (ex. '2010-25-12 00:00:01')~n~n"),
    io:format("UCengine (c) AF83 - http://ucengine.org~n"),
    {ok, nothing}.

getvalues([], _, _) ->
    [];
getvalues([Key|Keys], Args, [Default|Defaults]) ->
    case lists:keyfind(Key, 1, Args) of
	{_, ArgValue} ->
	    [ArgValue];
	false ->
	    [Default]
    end ++ getvalues(Keys, Args, Defaults).

getopt(Keys, Args) ->
    getopt(Keys, Args, array:to_list(array:new(length(Keys), {default, none}))).
getopt(Keys, Args, Defaults) ->
    Values = getvalues(Keys, Args, Defaults),
    RawRemaining = lists:filter(fun({ArgKey, _}) ->
					lists:member(ArgKey, Keys) == false
				end,
				Args),
    Remaining = [{Key, string:join(Value, " ")} || {Key, Value} <- RawRemaining],
    {Values, Remaining}.

json_escape(String) when is_list(String) ->
    re:replace(String, "\"", "\\\"", [{return, list}]);
json_escape(Integer) when is_integer(Integer) ->
    Integer;
json_escape(Atom) when is_atom(Atom)->
    String = atom_to_list(Atom),
    json_escape(String).

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

display_array_elems(json, [], Fields) ->
    nothing;
display_array_elems(json, [Record|Records], Fields) ->
    display(json, Record, Fields),
    case Records of
	[] ->
	    nothing;
	_ ->
	    io:format(",~n"),
	    display_array_elems(json, Records, Fields)
    end.
    
display(json, Records, Fields) when is_list(Records) ->
    io:format("["),
    display_array_elems(json, Records, Fields),
    io:format("]"),
    ok;
display(json, Record, Fields) ->
    [_|Values] = tuple_to_list(Record),
    io:format("{~n"),
    display_field(json, Values, Fields),
    io:format("}"),
    ok;

display(erlang, Record, _) ->
    io:format("~p~n", [Record]),
    ok.

call(Object, Action, Args) ->
    Module = list_to_atom("uce_" ++ atom_to_list(Object)),
    case rpc:call(default_node(), Module, Action, Args) of
	{badrpc, Reason} ->
	    {error, Reason};
	Result ->
	    Result
    end.

parse_date([Date, Time]) when is_list(Date), is_list(Time) ->
    parse_date(Date ++ " " ++ Time);
parse_date(Datetime) when is_list(Datetime) ->
    case string:tokens(Datetime, "- :") of
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
    end;
parse_date(none) ->
    0.

timestamp_to_iso(Militimestamp) when is_integer(Militimestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    Timestamp = Epoch + (Militimestamp div 1000),
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(Timestamp),
    Date = io_lib:format("~p-~p-~p ~p:~p:~p"
                        , [Year, Month, Day, Hours, Minutes, Seconds]),
    lists:flatten(Date).


success(Result) ->
    io:format("Success: ~p", [Result]),
    ok.

error(Reason) ->
    case Reason of
	nodedown ->
	    io:format("Fatal: UCengine node is not running, call 'ucectl start' to start it.");
	_ ->
	    io:format("Error: ~p", [Reason])
    end,
    error.

%%
%% Org
%%
action(org, add, Args) ->
    case getopt(["name"], Args) of
        {[[Name]], Metadata} ->
            case call(org, add, [#uce_org{name=Name, metadata=Metadata}]) of
            {ok, created} ->
                success(created);
            {error, Reason} ->
                error(Reason)
            end;
        {[none], _Metadata} ->
            error(missing_parameter)
    end;

action(org, get, Args) ->
    case getopt(["name"], Args) of
        {[[Name]], _} ->
            case call(org, get, [Name]) of
            {ok, Record} ->
                display(json, Record, record_info(fields, uce_org));
            {error, Reason} ->
                error(Reason)
            end;
        {[none], _} ->
            error(missing_parameter)
    end;

action(org, update, Args) ->
    case getopt(["name"], Args) of
        {[[Name]], Metadata} ->
            case call(org, update, [#uce_org{name=Name, metadata=Metadata}]) of
            {ok, updated} ->
                success(updated);
            {error, Reason} ->
                error(Reason)
            end;
        {[none], _Metadata} ->
            error(missing_parameter)
    end;

action(org, delete, Args) ->
    case getopt(["name"], Args) of
        {[[Name]], _} ->
            case call(org, delete, [Name]) of
            {ok, deleted} ->
                success(deleted);
            {error, Reason} ->
               error(Reason)
            end;
        {[none], _} ->
            error(missing_parameter)
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
    case getopt(["name", "org", "start", "end"], Args) of
        {[[Name], [Org], Start, End], Metadata} ->
            case call(meeting, add, [#uce_meeting{id=[Org, Name],
						  start_date=parse_date(Start),
						  end_date=parse_date(End),
						  metadata=Metadata}]) of
		{ok, created} ->
		    success(created);
		{error, Reason} ->
		    error(Reason)
            end;
        {[none, none, none, none], _Metadata} ->
            error(missing_parameter)
    end;

action(meeting, delete, Args) ->
    case getopt(["name", "org"], Args) of
        {[[Name], [Org]], _} ->
            case call(meeting, delete, [[Org, Name]]) of
		{ok, deleted} ->
		    success(deleted);
		{error, Reason} ->
		    error(Reason)
            end;
        {[none, none], _} ->
            error(missing_parameter)
    end;

action(meeting, get, Args) ->
    case getopt(["name", "org"], Args) of
        {[[Name], [Org]], _} ->
            case call(meeting, get, [[Org, Name]]) of
            {ok, Record} ->
                display(json, Record, record_info(fields, uce_meeting));
            {error, Reason} ->
                error(Reason)
            end;
        {[none, none], _} ->
            error(missing_parameter)
    end;

action(meeting, update, Args) ->
    case getopt(["name", "org", "start", "end"], Args) of
        {[[Name], [Org], Start, End], Metadata} ->
            case call(meeting, update, [#uce_meeting{id=[Org, Name],
                                 start_date=parse_date(Start),
                                 end_date=parse_date(End),
                                 metadata=Metadata}]) of
            {ok, updated} ->
                success(updated);
            {error, Reason} ->
                error(Reason)
            end;
        {[_Name, _Org, _Start, _End], _Metadata} ->
            error(missing_parameter)
    end;

action(meeting, list, Args) ->
    Res = case getopt(["org", "status"], Args) of
	      {[[Org], [Status]], _} ->
		  call(meeting, list, [Org, Status]);
	      {[[Org], none], _} ->
		  call(meeting, list, [Org, "all"]);
	      {[none, none], _} ->
            {error, missing_parameter}
	  end,
    case Res of
	{ok, Records} ->
	    display(json, Records, record_info(fields, uce_meeting));
	{error, Reason} ->
	    error(Reason)
    end;

%%
%% ACL
%%
action(acl, add, Args) ->
    case getopt(["uid", "org", "meeting", "object", "action"],
                Args,
                [none, [""], [""], none, none]) of
        {[[Uid], [Org], [Meeting], [Object], [Action]], Conditions} ->
            case call(acl, add, [#uce_acl{uid=Uid,
                          location=[Org, Meeting],
                          object=Object,
                          action=Action,
                          conditions=Conditions}]) of
            {ok, created} ->
                success(created);
            {error, Reason} ->
                error(Reason)
            end;
        {[_Uid, _Org, _Meeting, _Object, _Action], _Conditions} ->
            error(missing_parameter)
    end;

action(acl, delete, Args) ->
    case getopt(["uid", "org", "meeting", "object", "action"],
                Args, [none, [""], [""], none, none]) of
        {[[Uid], [Org], [Meeting], [Object], [Action]], Conditions} ->
            case call(acl, delete, [Uid, Object, Action, [Org, Meeting], Conditions]) of
            {ok, deleted} ->
                success(deleted);
            {error, Reason} ->
                error(Reason)
            end;
        {[_Uid, _Org, _Meeting, _Object, _Action], _Conditions} ->
            error(missing_parameter)
    end;

action(acl, check, Args) ->
	case getopt(["uid", "org", "meeting", "object", "action"],
                Args,
                [none, [""], [""], none, none]) of
        {[[Uid], [Org], [Meeting], [Object], [Action]], Conditions} ->
            case call(acl, check, [Uid, Object, Action, [Org, Meeting], Conditions]) of
            {ok, true} ->
                success(true);
            {ok, false} ->
                success(false);
            {error, Reason} ->
                error(Reason)
            end;
        {[_Uid, _Org, _Meeting, _Object, _Action], _Conditions} ->
            error(missing_parameter)
    end;

%%
%% Users
%%

action(user, add, Args) ->
    case getopt(["uid", "auth", "credential"], Args) of
        {[[Uid], [Auth], [Credential]], Metadata} ->
            case call(user, add, [#uce_user{uid=Uid,
                            auth=Auth,
                            credential=Credential,
                            metadata=Metadata}]) of
            {ok, created} ->
                success(created);
            {error, Reason} ->
                error(Reason)
            end;
        {[_Uid, _Auth, _Credential], _Metadata} ->
            error(missing_parameter)
    end;

action(user, delete, Args) ->
    case getopt(["uid"], Args) of
        {[[Uid]], _} ->
            case call(user, delete, [Uid]) of
            {ok, deleted} ->
                success(deleted);
            {error, Reason} ->
                error(Reason)
            end;
        {[none], _} ->
            error(missing_parameter)
    end;


action(user, get, Args) ->
    case getopt(["uid"], Args, [none]) of
        {[[Uid]], _} ->
            case call(user, get, [Uid]) of
            {ok, Record} ->
                display(json, Record, record_info(fields, uce_user));
            {error, Reason} ->
                error(Reason)
            end;
        {[none], _} ->
            error(missing_parameter)
    end;

action(user, update, Args) ->
    case getopt(["uid", "auth", "credential"], Args) of
        {[[Uid], [Auth], [Credential]], Metadata} ->
            case call(user, update, [#uce_user{uid=Uid,
                               auth=Auth,
                               credential=Credential,
                               metadata=Metadata}]) of
            {ok, updated} ->
                success(updated);
            {error, Reason} ->
                error(Reason)
            end;
        {[_Uid, _Auth, _Credential], _Metadata} ->
            error(missing_parameter)
    end;

action(user, list, _) ->
    case call(user, list, []) of
	{ok, Records} ->
	    display(json, Records, record_info(fields, uce_user));
	{error, Reason} ->
	    error(Reason)
    end;

%%
%% Time
%%

action(time, get, _) ->
    io:format("Server time: ~p", [utils:now()]),
    ok;

%%
%% Utils
%%
action(demo, start, Args) ->
    rpc:call(default_node(), demo, start, Args);

action(Object, _, _) ->
    usage(Object).
