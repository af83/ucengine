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

-export([start/0, stop/0, cmd/2]).

-export([infos/3, meeting/3, user/3, user/4, role/3, role/4, time/2]).

-export([parse_date/1, timestamp_to_iso/0, timestamp_to_iso/1]).

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
    [{Key, string:join(Value, " ")}] ++ args_to_dictionary(Tail);
args_to_dictionary([_|Tail]) ->
    [] ++ args_to_dictionary(Tail).

filter_node({"node", _Value}) ->
    false;
filter_node({_Name, _Value}) ->
    true.

%%
%% Parse date
%%
parse_date([Date, Time]) when is_list(Date), is_list(Time) ->
    parse_date(Date ++ " " ++ Time);

parse_date("0") ->
    0;
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
            throw({error, bad_date})
    end;
parse_date(none) ->
    0;
parse_date(0) ->
    0.

timestamp_to_iso() ->
    timestamp_to_iso(utils:now()).
timestamp_to_iso(Militimestamp) when is_integer(Militimestamp) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    Timestamp = Epoch + (Militimestamp div 1000),
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} =
        calendar:gregorian_seconds_to_datetime(Timestamp),
    Date = io_lib:format("~p-~p-~p ~p:~p:~p"
                         , [Year, Month, Day, Hours, Minutes, Seconds]),
    lists:flatten(Date).

success(Result) when is_list(Result) ->
    io:format("Success: ~s", [Result]),
    ok;
success(Result) ->
    io:format("Success: ~p", [Result]),
    ok.

error(Reason) ->
    case Reason of
        nodedown ->
            io:format("Fatal: U.C.Engine node is not running, call 'ucengine start' to start it.");
        _ ->
            io:format("Error: ~p~n", [Reason])
    end,
    error.

start() ->
    Command = init:get_arguments(),
    Args = lists:filter(fun filter_node/1, args_to_dictionary(Command)),
    try cmd(proplists:lookup(dummy, Command), Args) of
        ok ->
            io:format("~n"),
            init:stop(0);
        error ->
            io:format("~n"),
            init:stop(2);
        {ok, nothing} ->
            init:stop(0);
        {ok, Result} ->
            io:format(Result, []),
            init:stop(0)
    catch
        Exception ->
            io:format("Fatal: ~p~n", [Exception]),
            init:stop(2)
    end,
    halt().

cmd({dummy, [Domain, Object, Action]}, Args) ->
    %% TODO: check domain parameter here
    Fun = list_to_atom(Object),
    ?MODULE:Fun(Domain, Action, Args);
cmd({dummy, [Domain, Object, Action, Action2]}, Args) ->
    %% TODO: check domain parameter here
    Fun = list_to_atom(Object),
    ?MODULE:Fun(Domain, Action, Action2, Args);
cmd({dummy, [Object, Action]}, Args) ->
    Fun = list_to_atom(Object),
    ?MODULE:Fun(Action, Args);
cmd({dummy, _Other}, _Args) ->
    usage().

stop() ->
    ok.

usage() ->
    io:format("Usage:~n"),
    io:format("ucengine-admin <domain> <object> <action> [--<parameter> <value>]~n~n"),

    io:format("Meetings:~n"),
    io:format("\tmeeting add --name <name> --start <date> --end <date> [--<metadata> <value>]~n"),
    io:format("\tmeeting update --name <name> --start <date> --end <date> [--<metadata> <value>]~n"),
    io:format("\tmeeting get --name <name>~n"),
    io:format("\tmeeting delete --name <name>~n"),
    io:format("\tmeeting list --status <status>~n~n"),

    io:format("Users:~n"),
    io:format("\tuser add --name <name> --auth <auth> --credential <credential> [--<metadata> <value>]~n"),
    io:format("\tuser update [--name <name>|--uid <uid>] --auth <auth> --credential <credential> [--<metadata> <value>]~n"),
    io:format("\tuser get [--name <name>|--uid <uid>]~n"),
    io:format("\tuser delete [--name <name>|--uid <uid>]~n"),
    io:format("\tuser list~n"),
    io:format("\tuser role add [--name <name>|--uid <uid>] --role <role> [--location <location>]~n"),
    io:format("\tuser role delete [--name <name>|--uid <uid>] --role <role> [--location <location>]~n~n"),

    io:format("Roles:~n"),
    io:format("\trole add --name <name>~n"),
    io:format("\trole delete --name <name>~n"),
    io:format("\trole access add --name <name> --action <action> --object <object> [--<condition> <value>]~n"),
    io:format("\trole access delete --name <name> --action <action> --object <object> [--<condition> <value>]~n"),
    io:format("\trole access check --name <name> --action <action> --object <object> [--<condition> <value>]~n~n"),

    io:format("ucengine-admin time get~n~n"),

    io:format("Formatting:~n"),
    io:format("\t<date>: ISO8601 formatted date (ex. '2010-12-25 00:00:01')~n~n"),
    io:format("U.C.Engine (c) AF83 - http://ucengine.org~n"),
    {ok, nothing}.

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

%%
%% Infos get
%%
infos(Domain, "get", _) ->
    {ok, Infos} = call(infos, get, [Domain]),
    {ok, infos_helpers:pretty_print(Infos, flat)};

%%
%% Infos update
%%
infos(_Domain, "update", []) ->
    error(missing_parameter);
infos(Domain, "update", Metadata) ->
    {ok, updated} = call(infos, update, [Domain, #uce_infos{domain=Domain, metadata=Metadata}]),
    success(updated).

%%
%% Meeting add
%%
meeting(Domain, "add", Args) ->
    case proplists:split(Args, ["name", "start", "end"]) of
        {[[{"name", Name}], [{"start", Start}], [{"end", End}]], Metadata} ->
            {ok, created} = call(meeting, add, [Domain,
                                                #uce_meeting{id={Name, Domain},
                                                             start_date=parse_date(Start),
                                                             end_date=parse_date(End),
                                                             metadata=Metadata}]),
            success(created);
        _Other ->
            error(missing_parameter)
    end;

%%
%% Meeting delete
%%
meeting(Domain, "delete", [{"name", Name}]) ->
    {ok, deleted} = call(meeting, delete, [Domain, {Name, Domain}]),
    success(deleted);
meeting(_Domain, "delete", _Args) ->
    error(missing_parameter);

%%
%% Meeting get
%%
meeting(Domain, "get", [{"name", Name}]) ->
    {ok, Record} = call(meeting, get, [Domain, {Name, Domain}]),
    {ok, meeting_helpers:pretty_print(Record, flat)};
meeting(_Domain, "get", _Args) ->
    error(missing_parameter);

%%
%% Meeting update
%%
meeting(Domain, "update", Args) ->
    case proplists:split(Args, ["name", "start", "end"]) of
        {[[{"name", Name}], [{"start", Start}], [{"end", End}]], Metadata} ->
            {ok, updated} = call(meeting, update, [Domain,
                                                   #uce_meeting{id={Name, Domain},
                                                                start_date=parse_date(Start),
                                                                end_date=parse_date(End),
                                                                metadata=Metadata}]),
            success(updated);
        _Other ->
            error(missing_parameter)
    end;

%%
%% Meeting list
%%
meeting(Domain, "list", [{"status", Status}]) ->
    {ok, Records} = call(meeting, list, [Domain, Status]),
    {ok, meeting_helpers:pretty_print(Records, flat)};
meeting(_Domain, "list", _Args) ->
    error(missing_parameter).

%%
%% Users
%%
user(Domain, "add", Args) ->
    case proplists:split(Args, ["name", "auth", "credential"]) of
        {[[{"name", Name}], [{"auth", Auth}], [{"credential", Credential}]], Metadata} ->
            {ok, Uid} = call(user, add, [Domain,
                                         #uce_user{id={none, Domain},
                                                   name=Name,
                                                   auth=Auth,
                                                   credential=Credential,
                                                   metadata=Metadata}]),
            success(Uid);
        _Other ->
            error(missing_parameter)
    end;

%%
%% User delete
%%
user(Domain, "delete", [{"name", Name}]) ->
    {ok, #uce_user{id={Uid, _}}} = call(user, get, [Domain, Name]),
    user(Domain, "delete", [{"uid", Uid}]);
user(Domain, "delete", [{"uid", Uid}]) ->
    {ok, deleted} = call(user, delete, [Domain, {Uid, Domain}]),
    success(deleted);
user(_Domain, "delete", _Other) ->
    error(missing_parameter);

%%
%% User get
%%
user(Domain, "get", [{"name", Name}]) ->
    {ok, Record} = call(user, get, [Domain, Name]),
    {ok, user_helpers:pretty_print(Record, flat)};
user(Domain, "get", [{"uid", Uid}]) ->
    {ok, Record} = call(user, get, [Domain, {Uid, Domain}]),
    {ok, user_helpers:pretty_print(Record, flat)};
user(_Domain, "get", _Other) ->
    error(missing_parameter);

%%
%% User update
%%
user(Domain, "update", Args) ->
    {ok, Uid} = get_user_uid(Domain, Args),
    case proplists:split(Args, ["name", "auth", "credential"]) of
        {[[{"name", Name}], [{"auth", Auth}], [{"credential", Credential}]], Metadata} ->
            {ok, updated} = call(user, update, [Domain,
                                                #uce_user{id={Uid, Domain},
                                                          name=Name,
                                                          auth=Auth,
                                                          credential=Credential,
                                                          metadata=Metadata}]),
            success(updated);
        _Other ->
            error(missing_parameter)
    end;

%%
%% User list
%%
user(Domain, "list", _Args) ->
    {ok, Records} = call(user, list, [Domain]),
    {ok, user_helpers:pretty_print(Records, flat)}.

get_user_uid(Domain, Args) ->
    Uid = proplists:get_value("uid", Args),
    Name = proplists:get_value("name", Args),
    case {Uid, Name} of
        {Uid, _Name} when is_list(Uid) ->
            {ok, Uid};
        {_Uid, Name} when is_list(Name) ->
            {ok, #uce_user{id={TmpId, _}}} = call(user, get, [Domain, Name]),
            {ok, TmpId};
        _Other ->
            {error, not_found}
    end.

%%
%% User add role
%%
user(Domain, "role", "add", Args) ->
    {ok, Uid} = get_user_uid(Domain, Args),
    Role = proplists:get_value("role", Args, none),
    Location = proplists:get_value("location", Args, ""),
    case Role of
        none ->
            error(missing_parameter);
        Role ->
            {ok, updated} = call(user, add_role, [Domain,
                                                  {Uid, Domain},
                                                  {Role, Location}]),
            success(updated)
        end;

%%
%% User delete role
%%
user(Domain, "role", "delete", Args) ->
    {ok, Uid} = get_user_uid(Domain, Args),
    Role = proplists:get_value("role", Args, none),
    Location = proplists:get_value("location", Args, ""),
    case Role of
        none ->
            error(missing_parameter);
        Role ->
            {ok, updated} = call(user, delete_role, [Domain,
                                                     {Uid, Domain},
                                                     {Role, Location}]),
            success(updated)
    end.

%%
%% Role add
%%
role(Domain, "add", [{"name", Name}]) ->
    {ok, created} = call(role, add, [Domain, #uce_role{id={Name, Domain}}]),
    success(created);
role(_Domain, "add", _Other) ->
    error(missing_parameter);

%%
%% Role delete
%%
role(Domain, "delete", [{"name", Name}]) ->
    {ok, deleted} = call(role, delete, [Domain, {Name, Domain}]),
    success(deleted);
role(_Domain, "delete", _Other) ->
    error(missing_parameter).

%%
%% Role access add
%%
role(Domain, "access", "add", Args) ->
    case proplists:split(Args, ["name", "action", "object"]) of
        {[[{"name", Name}], [{"action", Action}], [{"object", Object}]], Conditions} ->
            {ok, updated} = call(role, add_access, [Domain, {Name, Domain},
                                                    #uce_access{action=Action,
                                                                object=Object,
                                                                conditions=Conditions}]),
            success(updated);
        _Other ->
            error(missing_parameter)
     end;

%%
%% Role access delete
%%
role(Domain, "access", "delete", Args) ->
    case proplists:split(Args, ["name", "action", "object"]) of
        {[[{"name", Name}], [{"action", Action}], [{"object", Object}]], Conditions} ->
            {ok, updated} = call(role, delete_access, [Domain, {Name, Domain},
                                                       #uce_access{action=Action,
                                                                   object=Object,
                                                                   conditions=Conditions}]),
            success(updated);
        _Other ->
            error(missing_parameter)
     end;

%%
%% Role access check
%%
role(Domain, "access", "check", Args) ->
    {ok, Uid} = get_user_uid(Domain, Args),
    Location = proplists:get_value("location", Args, ""),

    case proplists:split(Args, ["action", "object"]) of
        {[[{"action", Action}], [{"object", Object}]], Conditions} ->
            {ok, Result} = call(access, check, [Domain,
                                                {Uid, Domain},
                                                {Location, Domain},
                                                Object,
                                                Action,
                                                Conditions]),
            success(Result);
        _Other ->
            error(missing_parameter)
    end.

%%
%% Time
%%
time("get", _Args) ->
    io:format("Server time: ~p", [utils:now()]),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_arguments_test() ->
    ?assertEqual([], args_to_dictionary([])),
    ?assertEqual([{"node", "plop"}], args_to_dictionary([{'-node', ["plop"]}])),
    ?assertEqual([{"node", "plop plip"}], args_to_dictionary([{'-node', ["plop", "plip"]}])),
    ?assertEqual([{"node", "plop plip"}, {"name", "chuck"}], args_to_dictionary([{'-node', ["plop", "plip"]}, {'-name', ["chuck"]}])),
    ?assertEqual([{"node", "plop"}], args_to_dictionary([{'-node', ["plop"]}, {hello, "plop"}])).

parse_date_test() ->
    ?assertEqual(0, parse_date(0)),
    ?assertEqual(0, parse_date("0")),
    ?assertEqual(0, parse_date(none)),
    ?assertEqual(1306139400000, parse_date("2011-05-23 08:30:00")),
    ?assertEqual(1306139400000, parse_date(["2011-05-23", "08:30:00"])).

timestamp_to_iso_test() ->
    ?assertEqual("2011-5-23 8:30:0", timestamp_to_iso(1306139400000)).

-endif.
