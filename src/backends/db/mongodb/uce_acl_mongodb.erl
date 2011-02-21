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
-module(uce_acl_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_acl).

-export([add/1,
         delete/5,
         list/3]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_acl{}=ACL) ->
    case catch emongo:insert_sync(?MONGO_POOL, "uce_acl", to_collection(ACL)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, created}
    end.

delete({User, Domain}, Object, Action, {Location, _}, Conditions) ->
    case catch emongo:delete_sync(?MONGO_POOL, "uce_acl", [{"user", User},
                                                           {"domain", Domain},
                                                           {"object", Object},
                                                           {"action", Action},
                                                           {"location", Location},
                                                           {"conditions", Conditions}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, deleted}
    end.

list({User, Domain} = Uid, Object, Action) ->
    case catch emongo:find_all(?MONGO_POOL, "uce_acl", [{"user", User},
                                                        {"domain", Domain},
                                                        {"object", Object},
                                                        {"action", Action}]) of
        
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        ACLCollections ->
            ACL = lists:map(fun(Collection) ->
                                    from_collection(Collection)
                            end,
                            ACLCollections),
            {ok, AllActions} =
                case Action of
                    "all" ->
                        {ok, []};
                    _ ->
                        ?MODULE:list(Uid, Object, "all")
                end,
            {ok, AllObjects} =
                case Object of
                    "all" ->
                        {ok, []};
                    _ ->
                        ?MODULE:list(Uid, "all", Action)
                end,
            {ok, ACL ++ AllActions ++ AllObjects}
    end.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["user", "domain", "object", "action", "location", "conditions"]) of
        [User, Domain, Object, Action, Location, Conditions] ->
            #uce_acl{user={User, Domain},
                     action=Action,
                     object=Object,
                     location={Location, Domain},
                     conditions=Conditions};
        _ ->
            throw({error, bad_parameters})
    end.

to_collection(#uce_acl{user={User, Domain},
                       object=Object,
                       action=Action,
                       location={Location, _},
                       conditions=Conditions}) ->
    [{"user", User},
     {"domain", Domain},
     {"object", Object},
     {"action", Action},
     {"location", Location},
     {"conditions", Conditions}].
