%%
%%  U.C.Engine - Unified Collaboration Engine
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
-module(uce_meeting).

-export([add/2,
         delete/2,
         update/2,
         get/2,
         list/1,
         join/3,
         leave/3,
         roster/2,
         exists/2,
         publish/2,
         subscribe/7,
         unsubscribe/1]).

-include("uce.hrl").


-spec add(domain(), meeting()) -> {ok, created} | erlang:throw({error, conflict}).
add(Domain, #uce_meeting{id=Id} = Meeting) ->
    case exists(Domain, Id) of
        true ->
            throw({error, conflict});
        false ->
            (db:get(?MODULE, Domain)):add(Domain, Meeting)
    end.

-spec delete(domain(), meeting_id()) -> {ok, deleted} | erlang:throw({error, not_found}).
delete(Domain, Id) ->
    case exists(Domain, Id) of
        false ->
            throw({error, not_found});
        true ->
            (db:get(?MODULE, Domain)):delete(Domain, Id)
    end.

-spec get(domain(), meeting_id()) -> {ok, meeting()} | erlang:throw({error, not_found}).
get(Domain, Id) ->
    (db:get(?MODULE, Domain)):get(Domain, Id).

-spec update(domain(), meeting()) -> {ok, updated} | erlang:throw({error, not_found}).
update(Domain, #uce_meeting{id=Id} = Meeting) ->
    case exists(Domain, Id) of
        false ->
            throw({error, not_found});
        true ->
            (db:get(?MODULE, Domain)):update(Domain, Meeting)
    end.

-spec list(domain()) -> {ok, list(meeting)} | erlang:throw({error, bad_parameters}).
list(Domain) ->
    (db:get(?MODULE, Domain)):list(Domain).

-spec exists(domain(), meeting_id()) -> boolean().
exists(_Domain, "") ->
    true; % root
exists(Domain, Id) ->
    case catch get(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _} ->
            true
    end.

-spec join(domain(), meeting_id(), user_id()) -> {ok, updated} | erlang:throw({error, not_found}).
join(Domain, Id, User) ->
    case uce_user:exists(Domain, User) of
        false ->
            throw({error, not_found});
        true ->
            {ok, Meeting} = get(Domain, Id),
            case lists:member(User, Meeting#uce_meeting.roster) of
                false ->
                    update(Domain, Meeting#uce_meeting{roster=Meeting#uce_meeting.roster ++ [User]});
                true ->
                    {ok, updated}
            end
    end.

-spec leave(domain(), meeting_id(), user_id()) -> {ok, updated} | erlang:throw({error, not_found}).
leave(Domain, Id, User) ->
    case uce_user:exists(Domain, User) of
        false ->
            throw({error, not_found});
        true ->
            {ok, Meeting} = get(Domain, Id),
            case lists:member(User, Meeting#uce_meeting.roster) of
                false ->
                    throw({error, not_found});
                true ->
                    Roster = lists:subtract(Meeting#uce_meeting.roster, [User]),
                    update(Domain, Meeting#uce_meeting{roster=Roster})
            end
    end.

-spec roster(domain(), meeting_id()) -> list(user()) | erlang:throw({error, not_found}).
roster(Domain, Id) ->
    {ok, Meeting} = get(Domain, Id),
    {ok, Meeting#uce_meeting.roster}.


publish(Domain, Event) ->
    ?PUBSUB_MODULE:publish(Domain, Event).

subscribe(Pid, Domain, Uid, Location, From, Types, Parent) ->
    ?PUBSUB_MODULE:subscribe(Pid, Domain, Uid, Location, From, Types, Parent).

unsubscribe(Pid) ->
    ?PUBSUB_MODULE:unsubscribe(Pid).
