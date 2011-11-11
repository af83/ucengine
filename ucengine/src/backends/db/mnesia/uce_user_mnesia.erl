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
-module(uce_user_mnesia).

-export([init/0, drop/0]).

-export([add/2,
         delete/2,
         update/2,
         list/1,
         get/2,
         get_by_name/2]).

-include("uce.hrl").

init() ->
    case mnesia:create_table(uce_user,
                             [{disc_copies, [node()]},
                              {type, set},
                              {attributes, record_info(fields, uce_user)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, uce_user}} -> ok
    end.

add(Domain, #uce_user{id=Id} = User) ->
    case mnesia:dirty_write(User#uce_user{id={Id, Domain}}) of
        ok ->
            {ok, created}
    end.

delete(Domain, Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete({uce_user, {Id, Domain}})
                            end) of
        {atomic, ok} ->
            {ok, deleted}
    end.

update(Domain, #uce_user{id=Id} = User) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(User#uce_user{id={Id, Domain}})
                            end) of
        {atomic, _} ->
            {ok, updated}
    end.

list(Domain) ->
    case mnesia:dirty_match_object(#uce_user{id={'_', Domain},
                                             name='_',
                                             auth='_',
                                             credential='_',
                                             metadata='_',
                                             roles='_'}) of
        Users when is_list(Users) ->
            {ok, remove_domain_from_id(Users)}
    end.

get_by_name(Domain, Name) ->
    case mnesia:dirty_match_object(#uce_user{id={'_', Domain},
                                             name=Name,
                                             auth='_',
                                             credential='_',
                                             metadata='_',
                                             roles='_'}) of
        [User] ->
            {ok, remove_domain_from_id(User)};
        [] ->
            throw({error, not_found})
    end.

get(Domain, Id) ->
    case mnesia:dirty_read(uce_user, {Id, Domain}) of
        [User] ->
            {ok, remove_domain_from_id(User)};
        [] ->
            throw({error, not_found})
    end.

drop() ->
    mnesia:clear_table(uce_user).

remove_domain_from_id(Users) ->
    ?REMOVE_ID_FROM_RECORD(Users, uce_user).
