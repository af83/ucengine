%%
%%  U.C.Engine - Unified Colloboration Engine
%%  Copyright (C) 2011 af83
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU Affero General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU Affero General Public License for more details.
%%
%%  You should have received a copy of the GNU Affero General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
-module(uce_ctl).

-author('victor.goya@af83.com').

-export([start/0, stop/0, getopt/2, action/3, success/1, error/1]).
-export([parse_date/1, timestamp_to_iso/1]).

-compile({no_auto_import,[error/1]}).

-include("uce.hrl").

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
        Object == none ; Object == meeting ->
            io:format("Meetings:~n"),
            io:format("\tmeeting add --domain <domain> --name <name> --start <date> --end <date> [--<metadata> <value>]~n"),
            io:format("\tmeeting update --domain <domain> --name <name> --start <date> --end <date> [--<metadata> <value>]~n"),
            io:format("\tmeeting get --domain <domain> --name <name>~n"),
            io:format("\tmeeting delete --domain <domain> --name <name>~n"),
            io:format("\tmeeting list --domain <domain> --status <status>~n~n");
        true ->
            nothing
    end,
    if
        Object == none ; Object == user ->
            io:format("Users:~n"),
            io:format("\tuser add --domain <domain> --uid <uid> --auth <auth> --credential <credential> [--<metadata> <value>]~n"),
            io:format("\tuser update --domain <domain> --uid <uid> --auth <auth> --credential <credential> [--<metadata> <value>]~n"),
            io:format("\tuser get --domain <domain> --uid <uid>~n"),
            io:format("\tuser delete --domain <domain> --uid <uid>~n"),
            io:format("\tuser list --domain <domain>~n~n");
        true ->
            nothing
    end,
    if
        Object == none ; Object == acl ->
            io:format("ACL:~n"),
            io:format("\tacl add --domain <domain> --uid <uid> --object <object> --action <action> [--meeting <meeting>] [--condition <value>]~n"),
            io:format("\tacl delete --domain <domain> --uid <uid> --object <object> --action <action> [--meeting <meeting>] [--condition <value>]~n"),
            io:format("\tacl check --domain <domain> --uid <uid> --object <object> --action <action> [--meeting <meeting>] [--condition <value>]~n~n");
        true ->
            nothing
    end,
    io:format("Formatting:~n"),
    io:format("\t<date>: ISO8601 formatted date (ex. '2010-25-12 00:00:01')~n~n"),
    io:format("U.C.Engine (c) AF83 - http://ucengine.org~n"),
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

getopt([], Args) ->
    {[], [{Key, string:join(Value, " ")} || {Key, Value} <- Args]};
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

display_array_elems(json, [], _Fields) ->
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
    ok.

display(erlang, Record) ->
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
            io:format("Fatal: U.C.Engine node is not running, call 'ucectl start' to start it.");
        _ ->
            io:format("Error: ~p", [Reason])
    end,
    error.

%%
%% Meeting
%%
action(meeting, add, Args) ->
    case getopt(["domain", "name", "start", "end"], Args) of
        {[none, _, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, none, _, _], _Metadata} ->
            error(missing_parameter);
        {[[Domain], [Name], Start, End], Metadata} ->
            case call(meeting, add, [Domain, #uce_meeting{id=[Name],
                                                          start_date=parse_date(Start),
                                                          end_date=parse_date(End),
                                                          metadata=Metadata}]) of
                {ok, created} ->
                    success(created);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(meeting, delete, Args) ->
    case getopt(["domain", "name"], Args) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[[Domain], [Name]], _} ->
            case call(meeting, delete, [Domain, [Name]]) of
                {ok, deleted} ->
                    success(deleted);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(meeting, get, Args) ->
    case getopt(["domain", "name"], Args) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[[Domain], [Name]], _} ->
            case call(meeting, get, [Domain, [Name]]) of
                {ok, Record} ->
                    display(json, Record, record_info(fields, uce_meeting));
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(meeting, update, Args) ->
    case getopt(["domain", "name", "start", "end"], Args) of
        {[none, _, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, none, _, _], _Metadata} ->
            error(missing_parameter);
        {[[Domain], [Name], Start, End], Metadata} ->
            case call(meeting, update, [Domain, #uce_meeting{id=[Name],
                                                             start_date=parse_date(Start),
                                                             end_date=parse_date(End),
                                                             metadata=Metadata}]) of
                {ok, updated} ->
                    success(updated);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(meeting, list, Args) ->
    Res = case getopt(["domain", "status"], Args, [none, ["all"]]) of
              {[none, _], _} ->
                  {error, missing_parameter};
              {[[Domain], [Status]], _} ->
                  call(meeting, list, [Domain, Status])
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
    case getopt(["domain", "uid", "meeting", "object", "action"],
                Args, [none, none, [""], none, none]) of
        {[none, _, _, _, _], _} ->
            error(missing_parameter);
        {[_, none, _, _, _], _} ->
            error(missing_parameter);
        {[_, _, _, none, _], _} ->
            error(missing_parameter);
        {[_, _, _, _, none], _} ->
            error(missing_parameter);
        {[[Domain], [Uid], [Meeting], [Object], [Action]], Conditions} ->
            case call(acl, add, [Domain, #uce_acl{uid=Uid,
                                                  location=[Meeting],
                                                  object=Object,
                                                  action=Action,
                                                  conditions=Conditions}]) of
                {ok, created} ->
                    success(created);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(acl, delete, Args) ->
    case getopt(["domain", "uid", "meeting", "object", "action"],
                Args, [none, none, [""], none, none]) of
        {[none, _, _, _, _], _} ->
            error(missing_parameter);
        {[_, none, _, _, _], _} ->
            error(missing_parameter);
        {[_, _, _, none, _], _} ->
            error(missing_parameter);
        {[_, _, _, _, none], _} ->
            error(missing_parameter);
        {[[Domain], [Uid], [Meeting], [Object], [Action]], Conditions} ->
            case call(acl, delete, [Domain, Uid, Object, Action, [Meeting], Conditions]) of
                {ok, deleted} ->
                    success(deleted);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(acl, check, Args) ->
    case getopt(["domain", "uid", "meeting", "object", "action"],
                Args, [none, none, [""], none, none]) of
        {[none, _, _, _, _], _} ->
            error(missing_parameter);
        {[_, none, _, _, _], _} ->
            error(missing_parameter);
        {[_, _, _, none, _], _} ->
            error(missing_parameter);
        {[_, _, _, _, none], _} ->
            error(missing_parameter);
        {[[Domain], [Uid], [Meeting], [Object], [Action]], Conditions} ->
            case call(acl, check, [Domain, Uid, Object, Action, [Meeting], Conditions]) of
                {ok, true} ->
                    success(true);
                {ok, false} ->
                    success(false);
                {error, Reason} ->
                    error(Reason)
            end
    end;

%%
%% Users
%%

action(user, add, Args) ->
    case getopt(["domain", "uid", "auth", "credential"], Args) of
        {[none, _, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, none, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, _, none, _], _Metadata} ->
            error(missing_parameter);
        {[_, _, _, none], _Metadata} ->
            error(missing_parameter);
        {[[Domain], [Uid], [Auth], [Credential]], Metadata} ->
            case call(user, add, [Domain, #uce_user{uid=Uid,
                                                    auth=Auth,
                                                    credential=Credential,
                                                    metadata=Metadata}]) of
                {ok, created} ->
                    success(created);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(user, delete, Args) ->
    case getopt(["domain", "uid"], Args) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[[Domain], [Uid]], _} ->
            case call(user, delete, [Domain, Uid]) of
                {ok, deleted} ->
                    success(deleted);
                {error, Reason} ->
                    error(Reason)
            end
    end;


action(user, get, Args) ->
    case getopt(["domain", "uid"], Args, [none, none]) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[[Domain], [Uid]], _} ->
            case call(user, get, [Domain, Uid]) of
                {ok, Record} ->
                    display(json, Record, record_info(fields, uce_user));
                {error, Reason} ->
                    error(Reason)
            end;
        {[none], _} ->
            error(missing_parameter)
    end;

action(user, update, Args) ->
    case getopt(["domain", "uid", "auth", "credential"], Args) of
        {[none, _, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, none, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, _, none, _], _Metadata} ->
            error(missing_parameter);
        {[_, _, _, none], _Metadata} ->
            error(missing_parameter);
        {[[Domain], [Uid], [Auth], [Credential]], Metadata} ->
            case call(user, update, [Domain, #uce_user{uid=Uid,
                                                       auth=Auth,
                                                       credential=Credential,
                                                       metadata=Metadata}]) of
                {ok, updated} ->
                    success(updated);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(user, list, Args) ->
    case getopt(["domain"], Args) of
        {[none], _} ->
            error(missing_parameter);
        {[[Domain]], _} ->
            case call(user, list, [Domain]) of
                {ok, Records} ->
                    display(json, Records, record_info(fields, uce_user));
                {error, Reason} ->
                    error(Reason)
            end
    end;

%%
%% Time
%%

action(time, get, _) ->
    io:format("Server time: ~p", [utils:now()]),
    ok;

%%
%% Info
%%
action(infos, get, Args) ->
    case getopt(["domain"], Args) of
        {[none], _} ->
            error(missing_parameter);
        {[[Domain]], _} ->
            case call(infos, get, [Domain]) of
                {ok, Infos} ->
                    display(erlang, Infos);
                {error, Reason} ->
                    error(Reason)
            end
    end;

action(infos, update, Args) ->
    case getopt(["domain"], Args) of
        {[[Domain]], Metadata} ->
            case call(infos, update, [Domain, Metadata]) of
                {ok, updated} ->
                    success(updated);
                {error, Reason} ->
                    error(Reason)
            end;
        _ ->
            error(missing_parameter)
    end;

%%
%% Utils
%%
action(demo, start, Args) ->
    rpc:call(default_node(), demo, start, Args);

action(Object, _, _) ->
    usage(Object).
