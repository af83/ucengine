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

-export([listen/7, filter/5]).

-include("uce.hrl").

listen(Domain, Location, Search, From, Types, Parent, Timeout) ->
    receive
        {event, Event} ->
            case filter(Event, From, Types, Parent, Search) of
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


filter(Event, From, Types, Parent, Search) ->
    Filters = [fun filter_from/5,
               fun filter_types/5,
               fun filter_parent/5,
               fun filter_search/5],
    lists:all(fun(A) ->
                      A(Event, From, Types, Parent, Search)
              end, Filters).

filter_from(_Event, "", _Types, _Parent, _Search) ->
    true;
filter_from(Event, From, _Types, _Parent, _Search) ->
    Event#uce_event.from =:= From.

filter_types(_Event, _From, "", _Parent, _Search) ->
    true;
filter_types(Event, _From, Types, _Parent, _Search) ->
    lists:member(Event#uce_event.type, Types).

filter_parent(_Event, _From, _Types, "", _Search) ->
    true;
filter_parent(Event, _From, _Types, Parent, _Search) ->
    Event#uce_event.parent =:= Parent.

filter_search(_Event, _From, _Types, _Parent, "") ->
    true;
filter_search(#uce_event{metadata=Metadata},  _From, _Types, _Parent, Search) ->
    uce_event_erlang_search:search_metadata(Metadata, Search).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

filter_test() ->
    Event = #uce_event{
      id = 1234,
      datetime = 1234,
      location = "test",
      from = "test.user@af83.com",
      type = "test.event",
      parent = "another.user@af83.com",
      metadata = {struct, [{"description", "an nice event"}]}
     },
    ?assert(filter(Event, "", "", "", "")).

-endif.


