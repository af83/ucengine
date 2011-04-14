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
-module(uce_presence_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_presence).

-export([add/2,
         list/1,
         get/2,
         delete/2,
         update/2,
         all/1]).

-include("uce.hrl").
-include("mongodb.hrl").


%%--------------------------------------------------------------------
%% @spec (Domain, #uce_presence{}) -> {ok, created} | {error, bad_parameters}
%% @doc Insert given record #uce_presence{} in uce_presence mongodb table
%% @end
%%--------------------------------------------------------------------
add(Domain, #uce_presence{}=Presence) ->
    case catch emongo:insert_sync(Domain, "uce_presence", to_collection(Presence)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            %%{Id, Domain} = Presence#uce_presence.id,
            {ok, Presence#uce_presence.id}
    end.

%%--------------------------------------------------------------------
%% @spec ({User::list, Domain::list}) -> {ok, [#uce_presence{}, #uce_presence{}, ..] = Presences::list} | {error, bad_parameters}
%% @doc List all record #uce_presence for the given user and domain
%% @end
%%--------------------------------------------------------------------
list({User, Domain}) ->
    case catch emongo:find_all(Domain, "uce_presence", [{"user", User},
                                                        {"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        Collections ->
            Records = lists:map(fun(Collection) ->
                                        from_collection(Collection)
                                end,
                                Collections),
            {ok, Records}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list) -> {ok, [#uce_presence{}, #uce_presence{}, ..] = Presences::list} | {error, bad_parameters}
%% @doc List all record #uce_presence for the given domain
%% @end
%%--------------------------------------------------------------------
all(Domain) ->
    case catch emongo:find_all(Domain, "uce_presence", [{"domain", Domain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        Collections ->
            Records = lists:map(fun(Collection) ->
                                        from_collection(Collection)
                                end,
                                Collections),
            {ok, Records}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, {Sid::list, SDomain::list}) -> {ok, #uce_presence{}} | {error, bad_parameters} | {error, not_found}
%% @doc Get record uce_presence which correspond to the given id and domain
%% @end
%%--------------------------------------------------------------------
get(Domain, {SId, SDomain}) ->
    case catch emongo:find_one(Domain, "uce_presence", [{"id", SId}, {"domain", SDomain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [Collection] ->
            {ok, from_collection(Collection)};
        _ ->
            throw({error, not_found})
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, {Sid::list, SDomain::list}) -> {ok, deleted} | {error, bad_parameters}
%% @doc Delete record
%% @end
%%--------------------------------------------------------------------
delete(Domain, {SId, SDomain}) ->
    case catch emongo:delete_sync(Domain, "uce_presence", [{"id", SId}, {"domain", SDomain}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, deleted}
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_presence{}) -> {ok, updated} | {error, bad_parameters}
%% @doc Update record
%% @end
%%--------------------------------------------------------------------
update(Domain, #uce_presence{}=Presence) ->
    {Id, Domain} = Presence#uce_presence.id,
    case catch emongo:update_sync(Domain, "uce_presence",
                                  [{"id", Id}, {"domain", Domain}],
                                  to_collection(Presence), false) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, udpated}
    end.


%%--------------------------------------------------------------------
%% @spec ([{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list) -> #uce_presence{} | {error, bad_parameters}
%% @doc Convert collection returned by mongodb to valid record #uce_meeting{}
%% @end
%%--------------------------------------------------------------------
from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                   ["id", "domain", "user", "auth", "last_activity", "timeout", "metadata"]) of
        [Id, Domain, User, Auth, LastActivity, Timeout, Metadata] ->
            #uce_presence{id={Id, Domain},
                          user={User, Domain},
                          auth=Auth,
                          last_activity=list_to_integer(LastActivity),
                          timeout=list_to_integer(Timeout),
                          metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.

%%--------------------------------------------------------------------
%% @spec (#uce_presence{}) -> [{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list
%% @doc Convert #uce_presence{} record to valid collection
%% @end
%%--------------------------------------------------------------------
to_collection(#uce_presence{id={Id, Domain},
                            user={User, _},
                            auth=Auth,
                            last_activity=LastActivity,
                            timeout=Timeout,
                            metadata=Metadata}) ->
    [{"id", Id},
     {"domain", Domain},
     {"user", User},
     {"auth", Auth},
     {"last_activity", integer_to_list(LastActivity)},
     {"timeout", integer_to_list(Timeout)},
     {"metadata", Metadata}].
