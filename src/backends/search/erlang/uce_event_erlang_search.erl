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
-module(uce_event_erlang_search).

-author('victor.goya@af83.com').

-export([add/1, list/7]).

-include("uce.hrl").

-define(EVENT_DBMOD, (fun() ->
			      list_to_atom("uce_event_" ++ atom_to_list(config:get(db)))
		      end())).

add(_) ->
    {ok, created}.

search_value(_, []) ->
    true;
search_value(Value, [Word|Words]) ->
    case string:str(Value, Word) of
        0 ->
            false;
        _ ->
            search_value(Value, Words)
    end. 
search_metadata([], _) ->
    false;
search_metadata([{_, Value}|Tail], Words) ->
    case search_value(Value, Words) of
        true ->
            true;
        false ->
            search_metadata(Tail, Words)
    end.
search(Events, Words) ->
    lists:filter(fun(#uce_event{metadata=Metadata}) ->
			 search_metadata(Metadata, Words)
		 end,
		 Events).

list(Location, Search, From, Type, Start, End, Parent) ->
    {ok, Events} = ?EVENT_DBMOD:list(Location, From, Type, Start, End, Parent),
    {ok, search(Events, Search)}.

