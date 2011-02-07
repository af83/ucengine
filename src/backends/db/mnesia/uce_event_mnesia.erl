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
-module(uce_event_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_event).

-export([init/0, drop/0]).

-export([add/2,
         get/2,
         list/7]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_event,
			[{disc_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, uce_event)}]).

add(_Domain, #uce_event{} = Event) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Event)
			    end) of
	{atomic, _} ->
	    {ok, created};
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(_Domain, Id) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_event, Id)
			    end) of
	{aborted, Reason} ->
	    {error, Reason};
	{atomic, []} ->
	    {error, not_found};
	{atomic, [Event]} ->
	    {ok, Event}
    end.

list(_Domain, Location, From, Type, Start, End, Parent) ->
    SelectLocation = case Location of
			 [""] ->
			     ['$4'];
			 [Meeting] ->
			     [Meeting]
		     end,
    SelectFrom = if
		     From == '_' ->
			 '$5';
		     true ->
			 From
		 end,
    SelectType = if
		     Type == '_' ->
			 '$7';
		     true ->
			 Type
		 end,			  
    SelectParent = if
		       Parent == '_' ->
			   '$8';
		       true ->
			   Parent
		   end,
    Guard = if 
		Start /= 0, End /= infinity ->
		    [{'>=', '$2', Start}, {'=<', '$2', End}];
		
		Start /= 0 ->
		    [{'>=', '$2', Start}];
		
		End /= infinity ->
		    [{'=<', '$2', End}];
		
		true ->
		    []
	    end,
    Match = #uce_event{id='$1',
		       datetime='$2',
		       location=SelectLocation,
		       from=SelectFrom,
		       to='$6',
		       type=SelectType,
		       parent=SelectParent,
		       metadata='$9'},
    Result = {{'uce_event', '$1','$2', SelectLocation,
	       SelectFrom, '$6', SelectType, SelectParent, '$9'}},
    {ok, mnesia:dirty_select(uce_event, [{Match, Guard, [Result]}])}.

drop() ->
    mnesia:clear_table(uce_event).
