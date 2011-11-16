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
-module(event_helpers).

-include("uce.hrl").

-export([sort/1, sort/2]).

sort(Events) ->
    sort(Events, asc).
sort(Events, asc) ->
    lists:sort(fun(Event1, Event2) ->
                       Event1#uce_event.datetime < Event2#uce_event.datetime
               end,
               Events);
sort(Events, desc) ->
    lists:sort(fun(Event1, Event2) ->
                       Event1#uce_event.datetime > Event2#uce_event.datetime
               end,
               Events).
