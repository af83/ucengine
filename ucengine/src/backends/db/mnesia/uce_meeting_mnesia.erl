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
    case mnesia:create_table(uce_meeting,
                             [{disc_copies, [node()]},
                              {type, set},
                              {attributes, record_info(fields, uce_meeting)}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, uce_meeting}} -> ok
    end.

add(_Domain, #uce_meeting{} = Meeting) ->
    case mnesia:dirty_write(Meeting) of
        {aborted, _} ->
            throw({error, bad_parameters});
        ok ->
            {ok, created}
    end.

delete(Domain, Id) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete({uce_meeting, {Id, Domain}})
                            end) of
        {aborted, _} ->
            throw({error, bad_parameters});
        {atomic, ok} ->
            {ok, deleted}
    end.

get(Domain, Id) ->
    case mnesia:dirty_read(uce_meeting, {Id, Domain}) of
        [Record] ->
            {ok, Record};
        [] ->
            throw({error, not_found});
        {aborted, _} ->
            throw({error, bad_parameters})
    end.

update(_Domain, #uce_meeting{} = Meeting) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(Meeting)
                            end) of
        {aborted, _} ->
            throw({error, bad_parameters});
        {atomic, _} ->
            {ok, updated}
    end.

list(Domain) ->
    case mnesia:dirty_match_object(#uce_meeting{id={'_', Domain},
                                                start_date='_',
                                                end_date='_',
                                                roster='_',
                                                metadata='_'}) of
        {aborted, _} ->
            throw({error, bad_parameters});
        Meetings ->
            {ok, Meetings}
    end.

drop() ->
    mnesia:clear_table(uce_meeting).
