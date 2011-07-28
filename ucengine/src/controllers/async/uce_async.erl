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
-module(uce_async).

-author('victor.goya@af83.com').

-export([listen/7, filter/2]).

-include("uce.hrl").

listen(Domain, Location, Search, From, Types, Parent, Timeout) ->
    receive
        {event, Event} ->
            case filter(Search, Event) of
                false ->
                    listen(Domain, Location, Search, From, Types, Parent, Timeout);
                true ->
                    {ok, Event}
            end;
        Other ->
            ?WARNING_MSG("unattended message ~p", [Other]),
            {ok, []}
    after
        Timeout ->
            {ok, []}
    end.


filter("", _Event) ->
    true;
filter(Search, #uce_event{metadata=Metadata}) ->
    uce_event_erlang_search:search_metadata(Metadata, Search).
