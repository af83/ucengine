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
-module(uce_async_lp).

-export([wait/7]).

-include("uce.hrl").

wait(Domain, Location, Search, From, Types, Parent, []) ->
    Self = self(),
    spawn(fun() ->
                  ?PUBSUB_MODULE:subscribe(self(), Domain, Location, From, Types, Parent),
                  {ok, Event} = uce_async:listen(Domain,
                                                  Location,
                                                  Search,
                                                  From,
                                                  Types,
                                                  Parent,
                                                  (config:get(long_polling_timeout) * 1000)),
                  Event2 = case Event of
                               [] ->
                                   [];
                               Event ->
                                   [Event]
                           end,
                  JSONEvents = mochijson:encode({struct,
                                                 [{result,
                                                   json_helpers:to_json(Domain, Event2)}]}),
                  yaws_api:stream_chunk_deliver(Self, list_to_binary(JSONEvents)),
                  yaws_api:stream_chunk_end(Self),
                  ?PUBSUB_MODULE:unsubscribe(self())
          end),
    {streamcontent_with_timeout, "application/json", <<>>, infinity};
wait(Domain, _Location, _Search, _From, _Types, _Parent, PreviousEvents) ->
    json_helpers:json(Domain, PreviousEvents).
