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

-export([add/1,
         delete/1,
         update/1,
         get/1,
         list/2,
         join/2,
         leave/2,
         roster/1,
         exists/1]).

-include("uce.hrl").
-include("uce_models.hrl").

add(#uce_meeting{id=Id} = Meeting) ->
    case ?MODULE:exists(Id) of
        true ->
            throw({error, conflict});
        false ->
            ?DB_MODULE:add(Meeting)
    end.

delete(Id) ->
    case ?MODULE:exists(Id) of
        false ->
            throw({error, not_found});
        true ->
            ?DB_MODULE:delete(Id)
    end.

get(Id) ->
    ?DB_MODULE:get(Id).

update(#uce_meeting{id=Id} = Meeting) ->
    case ?MODULE:exists(Id) of
        false ->
            throw({error, not_found});
        true ->
            ?DB_MODULE:update(Meeting)
    end.

list(Domain, Status) ->
    {ok, Meetings} = ?DB_MODULE:list(Domain),
    if
        Status == "all";
        Status == "upcoming";
        Status == "opened";
        Status == "closed" ->
            Now = utils:now(),
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
                                                         End == ?NEVER_ENDING_MEETING ->
                                                             true;
                                                         Now =< End ->
                                                             true;
                                                         true ->
                                                             false
                                                     end;
                                                 false ->
                                                     false
                                             end;
                                         "closed" ->
                                             if
                                                 End == ?NEVER_ENDING_MEETING ->
                                                     false;
                                                 Now >= End ->
                                                     true;
                                                 true ->
                                                     false
                                             end;
                                         _ ->
                                             false
                                     end
                             end,
                             Meetings),
            {ok, FilteredMeetings};
        true ->
            throw({error, bad_parameters})
    end.

exists(Id) ->
    case Id of
        {"", _} -> % root
            true;
        _ ->
            case catch ?MODULE:get(Id) of
                {error, not_found} ->
                    false;
                {error, Reason} ->
                    throw({error, Reason});
                {ok, _} ->
                    true
            end
    end.

join(Id, User) ->
    case uce_user:exists(User) of
        false ->
            throw({error, not_found});
        true ->
            {ok, Meeting} = ?MODULE:get(Id),
            case lists:member(User, Meeting#uce_meeting.roster) of
                false ->
                    ?MODULE:update(Meeting#uce_meeting{roster=Meeting#uce_meeting.roster ++ [User]});
                true ->
                    {ok, updated}
            end
    end.

leave(Id, User) ->
    case uce_user:exists(User) of
        false ->
            throw({error, not_found});
        true ->
            {ok, Meeting} = ?MODULE:get(Id),
            case lists:member(User, Meeting#uce_meeting.roster) of
                false ->
                    throw({error, not_found});
                true ->
                    Roster = lists:subtract(Meeting#uce_meeting.roster, [User]),
                    ?MODULE:update(Meeting#uce_meeting{roster=Roster})
            end
    end.

roster(Id) ->
    {ok, Meeting} = ?MODULE:get(Id),
    {ok, Meeting#uce_meeting.roster}.

