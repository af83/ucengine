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
-module(uce_async).

-author('victor.goya@af83.com').

-export([listen/9]).

-include("uce.hrl").

listen(Domain, Location, Search, From, Types, Uid, Start, End, Parent) ->
    ?PUBSUB_MODULE:subscribe(self(), Domain, Location, Search, From, Types, Uid, Start, End, Parent),
    Res = receive
              % TODO: filter messages in _Message according to the request criterias.
              % For now _Message is ignored and the whole thing is used as a
              % callback to retrieve new events from the database.
              {message, _Message} ->
                  {ok, Events} = uce_event:list(Domain,
                                                Location,
                                                Search,
                                                From,
                                                Types,
                                                Uid,
                                                Start,
                                                End,
                                                Parent,
                                                0,
                                                infinity,
                                                asc),
                  JSONEvents = mochijson:encode({struct,
                                                 [{result,
                                                   json_helpers:to_json(Domain, Events)}]}),
                  {ok, JSONEvents};
              Other ->
                  ?WARNING_MSG("unattended message ~p", [Other]),
                  {ok, []}
          after
              config:get(long_polling_timeout) * 1000 ->
                  JSONEmpty = mochijson:encode({struct, [{result, {array, []}}]}),
                  {ok, JSONEmpty}
          end,
    ?PUBSUB_MODULE:unsubscribe(self()),
    Res.
