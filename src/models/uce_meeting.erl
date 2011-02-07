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
-module(uce_meeting).

-author('victor.goya@af83.com').

-export([add/2, delete/2, update/2, get/2, list/2, join/3, leave/3, roster/2, exists/2]).

-include("uce.hrl").
-include("uce_models.hrl").

add(Domain, #uce_meeting{} = Meeting) ->
    case ?MODULE:exists(Domain, Meeting#uce_meeting.id) of
	true ->
	    {error, conflict};
	false ->
	    [_] = Meeting#uce_meeting.id,
            ?DB_MODULE:add(Domain, Meeting)
    end.

delete(Domain, Id) ->
    case ?MODULE:exists(Domain, Id) of
	false ->
	    {error, not_found};
	true ->
	    ?DB_MODULE:delete(Domain, Id)
    end.


get(Domain, Id) ->
    ?DB_MODULE:get(Domain, Id).

update(Domain, #uce_meeting{} = Meeting) ->
    case ?MODULE:get(Domain, Meeting#uce_meeting.id) of
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    ?DB_MODULE:update(Domain, Meeting)
    end.

list(Domain, Status) ->
    Now = utils:now(),
    case ?DB_MODULE:list(Domain) of
        {error, Reason} ->
            {error, Reason};
        {ok, Meetings} ->
            if
                Status == "all"; Status=="upcoming"; Status=="opened"; Status=="closed" ->
                    FilteredMeetings =
                        lists:filter(fun(#uce_meeting{start_date=Start, end_date=End}) ->
                                             case Status of
                                                 "all" ->
                                                     true;
                                                 "upcoming" ->
                                                     Now < Start;
                                                 "opened" ->
                                                     case Now >= Start of
                                                         true ->
                                                             if
                                                                 End == ?NEVER_ENDING_MEETING -> true;
                                                                 Now =< End -> true;
                                                                 true -> false
                                                             end;
                                                         false ->
                                                             false
                                                     end;
                                                 "closed" ->
                                                     if
                                                         End == ?NEVER_ENDING_MEETING -> false;
                                                         Now >= End -> true;
                                                         true -> false
                                                     end;
                                                 _ ->
                                                     false
                                             end
                                     end,
                                     Meetings),
                    {ok, FilteredMeetings};
                true ->
                    {error, bad_parameters}
            end
    end.

exists(Domain, Id) ->
    case ?MODULE:get(Domain, Id) of
	{error, _} ->
	    false;
	_ ->
	    true
    end.

join(Domain, Id, Uid) when is_list(Uid) ->
    case uce_user:exists(Domain, Uid) of
	false ->
	    {error, not_found};
	true ->
	    case ?MODULE:get(Domain, Id) of
		{error, Reason} ->
		    {error, Reason};
		{ok, #uce_meeting{} = Meeting} ->
		    case lists:member(Uid, Meeting#uce_meeting.roster) of
			false ->
			    Roster = Meeting#uce_meeting.roster ++ [Uid],
			    ?MODULE:update(Domain, Meeting#uce_meeting{roster=Roster});
			true ->
			    {ok, updated}
		    end
	    end
    end.

leave(Domain, Id, Uid) when is_list(Uid) ->
    case uce_user:exists(Domain, Uid) of
	false ->
	    {error, not_found};
	true ->
	    case ?MODULE:get(Domain, Id) of
		{error, Reason} ->
		    {error, Reason};
		{ok, #uce_meeting{} = Meeting} ->
		    case lists:member(Uid, Meeting#uce_meeting.roster) of
			false ->
			    {error, not_found};
			true ->
			    Roster = lists:subtract(Meeting#uce_meeting.roster, [Uid]),
			    ?MODULE:update(Domain, Meeting#uce_meeting{roster=Roster})
		    end
	    end
    end.

roster(Domain, Id) ->
    case ?MODULE:get(Domain, Id) of
	{error, Reason} ->
	    {error, Reason};
	{ok, #uce_meeting{} = Meeting} ->
	    {ok, Meeting#uce_meeting.roster}
    end.
