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
-module(uce_role_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_role).

-export([add/2,
         delete/2,
         update/2,
         get/2]).

-include("uce.hrl").
-include("mongodb.hrl").

add(Domain, #uce_role{} = Role) ->
    mongodb_helpers:ok(emongo:insert_sync(Domain, "uce_role", to_collection(Role))),
    {ok, created}.

update(Domain, #uce_role{id={Name, _}} = Role) ->
    mongodb_helpers:updated(emongo:update_sync(Domain, "uce_role", [{"name", Name},
                                                                    {"domain", Domain}],
                                               to_collection(Role), false)),
    {ok, updated}.

delete(Domain, {Name, Domain}) ->
    mongodb_helpers:ok(emongo:delete_sync(Domain, "uce_role", [{"name", Name},
                                                               {"domain", Domain}])),
    {ok, deleted}.

get(Domain, {Name, Domain}) ->
    case emongo:find_one(Domain, "uce_role", [{"name", Name},
                                              {"domain", Domain}]) of
        [Record] ->
            {ok, from_collection(Record)};
        [] ->
            throw({error, not_found})
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["name", "domain", "acl"]) of
        [Name, Domain, ACL] ->
            #uce_role{id={Name, Domain},
                      acl=[#uce_access{object=Object, action=Action, conditions=Conditions} ||
                              [Object, Action, Conditions] <- ACL]};
        _ ->
            throw({error, bad_parameters})
    end.

to_collection(#uce_role{id={Name, Domain},
                        acl=ACL}) ->
    [{"name", Name},
     {"domain", Domain},
     {"acl", [[Object, Action, Conditions] || #uce_access{object=Object,
                                                          action=Action,
                                                          conditions=Conditions} <- ACL]}].
