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
-module(uce_meeting_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_meeting).

-export([init/0, drop/0]).

-export([add/2,
         delete/2,
         get/2,
         update/2,
         list/1]).

-include("uce.hrl").

init() ->
    mnesia:create_table(uce_meeting,
			[{disc_copies, [node()]},
			 {type, set},
			 {attributes, record_info(fields, uce_meeting)}]).

add(_Domain, #uce_meeting{} = Meeting) ->
    case mnesia:transaction(fun() ->
			       mnesia:write(Meeting)
		       end) of
	{atomic, _} ->
	    {ok, created};
	{aborted, Reason} ->
	    {error, Reason}
    end.

delete(_Domain, Id) ->
    case mnesia:transaction(fun() ->
				    mnesia:delete({uce_meeting, Id})
			    end) of
	{atomic, ok} ->
	    {ok, deleted};
	{aborted, Reason} ->
	    {error, Reason}
    end.

get(_Domain, Id) ->
    case mnesia:transaction(fun() ->
				    mnesia:read(uce_meeting, Id)
			    end) of
	{atomic, [Record]} ->
	    {ok, Record};
	{atomic, _} ->
	    {error, not_found};
	{aborted, Reason} ->
	    {error, Reason}
    end.

update(_Domain, #uce_meeting{} = Meeting) ->
    case mnesia:transaction(fun() ->
				    mnesia:write(Meeting)
			    end) of
	{atomic, _} ->
	    {ok, updated};
	{aborted, Reason} ->
	    {error, Reason}
    end.

list(_Domain) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_meeting{id=['_'],
								     start_date='_',
								     end_date='_',
								     roster='_',
								     metadata='_'})
			    end) of
	{atomic, Meetings} ->
	    {ok, Meetings};
	{aborted, Reason} ->
	    {error, Reason}
    end.

drop() ->
    mnesia:clear_table(uce_meeting).
