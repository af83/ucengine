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
-module(uce_event_mongodb).

-author('victor.goya@af83.com').

-behaviour(gen_uce_event).

-export([add/1,
         get/2,
         list/6]).

-include("uce.hrl").
-include("mongodb.hrl").

add(#uce_event{domain=Domain} = Event) ->
    case catch emongo:insert_sync(Domain, "uce_event", to_collection(Event)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        _ ->
            {ok, Event#uce_event.id}
    end.

get(Domain, Id) ->
    case catch emongo:find_one(Domain, "uce_event", [{"id", Id}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p~n", [Reason]),
            throw({error, bad_parameters});
        [Collection] ->
            {ok, from_collection(Collection)};
        _ ->
            throw({error, not_found})
    end.

list({_M, Domain}=Location, From, Type, Start, End, Parent) ->
    SelectLocation = case Location of
                         {"", Domain} ->
                             [];
                         {Meeting, Domain} ->
                             [{"meeting", Meeting}]
                     end,
    SelectFrom = case From of
                     {"", _} ->
                         [];
                     {Uid, _} ->
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
                       emongo:find_all(Domain,"uce_event",
                                       SelectLocation ++
                                           SelectFrom ++
                                           SelectTypes ++
                                           SelectParent ++
                                           SelectTime,
                                       [{orderby, [{"this.datetime", asc}]}])),
    {ok, Events}.

from_collection(Collection) ->
    case utils:get(mongodb_helpers:collection_to_list(Collection),
                  ["id", "domain", "meeting", "from", "metadata", "datetime", "type", "parent", "to"]) of
        [Id, Domain, Meeting, From, Metadata, Datetime, Type, Parent, To] ->
            #uce_event{id=Id,
                       domain=Domain,
                       datetime=Datetime,
                       from={From, Domain},
                       to={To, Domain},
                       location={Meeting, Domain},
                       type=Type,
                       parent=Parent,
                       metadata=Metadata};
        _ ->
            throw({error, bad_parameters})
    end.

to_collection(#uce_event{domain=Domain,
                         id=Id,
                         location={Meeting, _},
                         from={From, Domain},
                         to={To, _},
                         metadata=Metadata,
                         datetime=Datetime,
                         type=Type,
                         parent=Parent}) ->
    [{"domain", Domain},
     {"id", Id},
     {"meeting", Meeting},
     {"from", From},
     {"to", To},
     {"metadata", Metadata},
     {"datetime", Datetime},
     {"type", Type},
     {"parent", Parent}].
