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

get_node() ->
    Command = init:get_arguments(),
    case utils:get(Command, ['-node']) of
        [none] ->
            NodeDomain =
                case re:run(atom_to_list(node()), "@(.*)", [{capture, all, list}]) of
                    {match, [_, Domain]} ->
                        Domain;
                    _ ->
                        "localhost"
                end,
            list_to_atom("ucengine@" ++ NodeDomain);
        [[Node]] ->
            list_to_atom(Node)
    end.

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
    io:format("Formatting:~n"),
    io:format("\t<date>: ISO8601 formatted date (ex. '2010-25-12 00:00:01')~n~n"),
    io:format("U.C.Engine (c) AF83 - http://ucengine.org~n"),
    {ok, nothing}.

getvalues([], _, _) ->
    [];
getvalues([Key|Keys], Args, [Default|Defaults]) ->
    case lists:keyfind(Key, 1, Args) of
        {_, ArgValue} ->
            [string:join(ArgValue, " ")];
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

json_escape({Id, Domain}) ->
    io:format(" [~p, ~p]", [Id, Domain]);    
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

display_roles(json, []) ->
    [];
display_roles(json, [{Role, Location}|Tail]) ->
    io:format("        ~p: ~p", [json_escape(Role), json_escape(Location)]),
    case Tail of
        [] ->
            io:format("~n");
        _ ->
            io:format(",~n")
    end,
    display_roles(json, Tail).

display_field(json, [], []) ->
    [];
display_field(json, [Value|Values], [Field|Fields]) ->
    case Field of
        metadata ->
            io:format("    \"metadata\": {~n"),
            display_metadata(json, Value),
            io:format("    }");
        roles ->
            io:format("    \"metadata\": {~n"),
            display_roles(json, Value),
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

%% display(erlang, Record) ->
%%     io:format("~p~n", [Record]),
%%     ok.

call(Object, Action, Args) ->
    Module = list_to_atom("uce_" ++ atom_to_list(Object)),
    case catch rpc:call(get_node(), Module, Action, Args) of
        {badrpc, Reason} ->
            throw({error, Reason});
        {error, Reason} ->
            throw({error, Reason});
        Result ->
            Result
    end.

parse_date([Date, Time]) when is_list(Date), is_list(Time) ->
    parse_date(Date ++ " " ++ Time);
parse_date(Datetime) when is_list(Datetime) ->
    case string:tokens(Datetime, "- :") of
        [Year, Month, Day, Hours, Minutes, Seconds] ->
            DateTime = {{list_to_integer(Year),
                         list_to_integer(Day),
                         list_to_integer(Month)},
                        {list_to_integer(Hours),
                         list_to_integer(Minutes),
                         list_to_integer(Seconds)}},
            Epoch =
                calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
            (calendar:datetime_to_gregorian_seconds(DateTime) - Epoch) * 1000;
        _ ->
            throw({error, bad_date})
    end;
parse_date(none) ->
    0.

timestamp_to_iso(Militimestamp) when is_integer(Militimestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    Timestamp = Epoch + (Militimestamp div 1000),
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(Timestamp),
    Date = io_lib:format("~p-~p-~p ~p:~p:~p"
                         , [Year, Day, Month, Hours, Minutes, Seconds]),
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
        {[Domain, Name, Start, End], Metadata} ->
            {ok, created} = call(meeting, add, [Domain, 
                                                #uce_meeting{id={Name, Domain},
                                                             start_date=parse_date(Start),
                                                             end_date=parse_date(End),
                                                             metadata=Metadata}]),
            success(created)
    end;

action(meeting, delete, Args) ->
    case getopt(["domain", "name"], Args) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[Domain, Name], _} ->
            {ok, deleted} = call(meeting, delete, [Domain, {Name, Domain}]),
            success(deleted)
    end;

action(meeting, get, Args) ->
    case getopt(["domain", "name"], Args) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[Domain, Name], _} ->
            {ok, Record} = call(meeting, get, [Domain, {Name, Domain}]),
            display(json, Record, record_info(fields, uce_meeting))
    end;

action(meeting, update, Args) ->
    case getopt(["domain", "name", "start", "end"], Args) of
        {[none, _, _, _], _Metadata} ->
            error(missing_parameter);
        {[_, none, _, _], _Metadata} ->
            error(missing_parameter);
        {[Domain, Name, Start, End], Metadata} ->
            {ok, updated} = call(meeting, update, [Domain, 
                                                   #uce_meeting{id={Name, Domain},
                                                                start_date=parse_date(Start),
                                                                end_date=parse_date(End),
                                                                metadata=Metadata}]),
            success(updated)
    end;

action(meeting, list, Args) ->
    case getopt(["domain", "status"], Args, [none, "all"]) of
        {[none, _], _} ->
            error(missing_parameter);
        {[Domain, Status], _} ->
            {ok, Records} = call(meeting, list, [Domain, Status]),
            display(json, Records, record_info(fields, uce_meeting))
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
        {[Domain, Name, Auth, Credential], Metadata} ->
            {ok, created} = call(user, add, [Domain,
                                             #uce_user{id={Name, Domain},
                                                       auth=Auth,
                                                       credential=Credential,
                                                       metadata=Metadata}]),
           success(created)
    end;

action(user, delete, Args) ->
    case getopt(["domain", "uid"], Args) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[Domain, Name], _} ->
            {ok, deleted} = call(user, delete, [Domain, {Name, Domain}]),
            success(deleted)
    end;


action(user, get, Args) ->
    case getopt(["domain", "uid"], Args, [none, none]) of
        {[none, _], _} ->
            error(missing_parameter);
        {[_, none], _} ->
            error(missing_parameter);
        {[Domain, Name], _} ->
            {ok, Record} = call(user, get, [Domain, {Name, Domain}]),
            display(json, Record, record_info(fields, uce_user));
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
        {[Domain, Name, Auth, Credential], Metadata} ->
            {ok, updated} = call(user, update, [Domain,
                                                #uce_user{id={Name, Domain},
                                                          auth=Auth,
                                                          credential=Credential,
                                                          metadata=Metadata}]),
            success(updated)
    end;

action(user, list, Args) ->
    case getopt(["domain"], Args) of
        {[none], _} ->
            error(missing_parameter);
        {[Domain], _} ->
            {ok, Records} = call(user, list, [Domain]),
            display(json, Records, record_info(fields, uce_user))
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
        {[Domain], _} ->
            {ok, Infos} = call(infos, get, [Domain]),
            display(json, Infos, record_info(fields, uce_infos))
    end;

action(infos, update, Args) ->
    case getopt(["domain"], Args) of
        {[Domain], Metadata} ->
            {ok, updated} = call(infos, update, [Domain, #uce_infos{domain=Domain, metadata=Metadata}]),
            success(updated);
        _ ->
            error(missing_parameter)
    end;

%%
%% Utils
%%
action(demo, start, _) ->
    rpc:call(get_node(), demo, start, []),
    success(started);

action(Object, _, _) ->
    usage(Object).
