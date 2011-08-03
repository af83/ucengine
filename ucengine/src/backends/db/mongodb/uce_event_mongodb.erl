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
-module(uce_event_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_event).

-export([add/2,
         get/2,
         list/8,
         index/1]).

-include("uce.hrl").
-include("mongodb.hrl").

%%--------------------------------------------------------------------
%% @spec (Domain::list, #uce_event{}) -> {ok, Id}
%% @doc Insert given record #uce_event{} in uce_event mongodb table
%% @end
%%--------------------------------------------------------------------
add(Domain, #uce_event{id=Id} = Event) ->
    mongodb_helpers:ok(emongo:insert_sync(Domain, "uce_event", to_collection(Domain, Event))),
    {ok, Id}.

%%--------------------------------------------------------------------
%% @spec (Domain::list, EventId::list) -> {ok, #uce_event{}} | {error, bad_parameters}
%% @doc Retrieve record #uce_event{} for the given id and domain
%% @end
%%--------------------------------------------------------------------
get(Domain, EventId) ->
    case emongo:find_one(Domain, "uce_event", [{"id", EventId}, {"domain", Domain}]) of
        [Collection] ->
            {ok, from_collection(Collection)};
        [] ->
            throw({error, not_found})
    end.

%%--------------------------------------------------------------------
%% @spec (Domain::list, location::list, FromId::list, Type::list, Start::(integer|atom), End::({integer|atom), Parent::list, Order::atom) -> {ok, [#uce_event{}, #uce_event{}, ...] = Events::list}
%% @doc Returns list of record #uce_event which are match with given keys
%% @end
%%--------------------------------------------------------------------
list(Domain, Location, From, Type, Start, End, Parent, Order) ->
    SelectLocation = case Location of
                         "" ->
                             [];
                         Meeting ->
                             [{"meeting", Meeting}]
                     end,
    SelectFrom = case From of
                     "" ->
                         [];
                     Uid ->
                         [{"from", Uid}]
                 end,
    SelectTypes = if
                     Type == [] ->
                         [];
                     true ->
                         [{"type", [{in, Type}]}]
                 end,
    SelectParent = if
                       Parent == "" ->
                           [];
                       true ->
                           [{"parent", Parent}]
                   end,
    SelectTime = if
                     Start == 0, End == infinity ->
                         [];
                     Start /= 0, End == infinity ->
                         [{"datetime", [{'>=', Start}]}];
                     Start /= 0, End /= infinity ->
                         [{"datetime", [{'>=', Start},
                                        {'=<', End}]}];
                     Start == 0, End /= infinity ->
                         [{"datetime", [{'=<', End}]}];
                     true ->
                         []
                 end,
    Events = lists:map(fun(Collection) ->
                               from_collection(Collection)
                       end,
                       emongo:find_all(Domain, "uce_event",
                                       SelectLocation ++
                                           SelectFrom ++
                                           SelectTypes ++
                                           SelectParent ++
                                           SelectTime,
                                       [{orderby, [{"datetime", Order}]}])),
    {ok, Events}.

%%--------------------------------------------------------------------
%% @spec ([{Key::list, Value::list}, {Key::list, Value::list}, ...] = Collection::list) -> #uce_user{} | {error, bad_parameters}
%% @doc Convert collection returned by mongodb to valid record #uce_event{}
%% @end
%%--------------------------------------------------------------------
from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
              ["id", "domain", "meeting", "from", "metadata", "datetime", "type", "parent", "to"]) of
        [Id, _Domain, Meeting, From, Metadata, Datetime, Type, Parent, To] ->
            #uce_event{id=Id,
                       datetime=Datetime,
                       from=From,
                       to=To,
                       location=Meeting,
                       type=Type,
                       parent=Parent,
                       metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.

-spec to_collection(domain(), #uce_event{}) -> list({string(), any()}).
to_collection(Domain, #uce_event{id=Id,
                                 location=Meeting,
                                 from=From,
                                 to=To,
                                 metadata=Metadata,
                                 datetime=Datetime,
                                 type=Type,
                                 parent=Parent}) ->
    [{"domain", Domain},
     {"id", Id},
     {"meeting", Meeting},
     {"from", From},
     {"to", To},
     {"metadata", mongodb_helpers:to_bson(Metadata)},
     {"datetime", Datetime},
     {"type", Type},
     {"parent", Parent}].

-spec index(domain()) -> ok.
index(Domain) ->
    Indexes = [{"domain", 1},
               {"meeting", 1},
               {"from", 1},
               {"parent", 1},
               {"datetime", 1},
               {"type", 1}],
    [emongo:ensure_index(Domain, "uce_event", [Index]) || Index <- Indexes],
    ok.
