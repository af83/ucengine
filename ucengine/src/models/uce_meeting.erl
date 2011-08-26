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
-module(uce_meeting).

-author('victor.goya@af83.com').

% Meeting API
-export([add/2,
         delete/2,
         update/2,
         get/2,
         list/2,
         join/3,
         leave/3,
         roster/2,
         exists/2,
         publish/3,
         subscribe/3,
         unsubscribe/3,
         get_event_manager/2]).

-include("uce.hrl").


% FIXME: the spec miss the errors
-spec add(domain(), #uce_meeting{}) -> {ok, created}.
add(Domain, Meeting) ->
    Id = Meeting#uce_meeting.id,
    case uce_meeting_sup:start_child(Domain, Id) of
        {ok, _EventManager} ->
            case exists(Domain, Id) of
                false ->
                    (db:get(?MODULE, Domain)):add(Domain, Meeting),
                    % Make the root meeting subscribe to the newly created
                    % meeting
                    EventManager = get_event_manager(Domain, ""),
                    subscribe_meeting(Domain, Id, EventManager),
                    {ok, created};
                true ->
                    throw({error, conflict})
            end;
        {error, Reason} ->
            {error, Reason}
    end.

delete(Domain, Id) ->
    case exists(Domain, Id) of
        false ->
            throw({error, not_found});
        true ->
            uce_meeting_sup:terminate_child(Domain, Id),
            (db:get(?MODULE, Domain)):delete(Domain, Id)
    end.

-spec get(domain(), meeting()) -> {ok, #uce_meeting{}}.
get(Domain, Id) ->
    (db:get(?MODULE, Domain)):get(Domain, Id).

update(Domain, #uce_meeting{id=Id} = Meeting) ->
    assert_exists(Domain, Id),
    (db:get(?MODULE, Domain)):update(Domain, Meeting).

list(Domain, Status) ->
    {ok, Meetings} = (db:get(?MODULE, Domain)):list(Domain),
    % [FIXME] kill if, puts the throw in the last case or something like that.
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

%% @deprecated
exists(_Domain, "") ->
    true; % root
exists(Domain, Id) ->
    case catch get(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _} ->
            true
    end.

assert_exists(_Domain, "") ->
    ok;
assert_exists(Domain, Id) ->
    case catch get(Domain, Id) of
        {error, not_found} ->
            throw({error, not_found});%, "User " ++ Id ++ " is not found";
        {error, Reason} ->
            throw({error, Reason});
        {ok, _} ->
            ok
    end.

join(Domain, Id, User) ->
    assert_exists(Domain, Id),
    {ok, Meeting} = get(Domain, Id),
    case lists:member(User, Meeting#uce_meeting.roster) of
        false ->
            update(Domain, Meeting#uce_meeting{roster=Meeting#uce_meeting.roster ++ [User]});
        true ->
            {ok, updated}
    end.

leave(Domain, Id, User) ->
    uce_user:assert_exists(Domain, User),
    {ok, Meeting} = get(Domain, Id),
    case lists:member(User, Meeting#uce_meeting.roster) of
        false ->
            throw({error, not_found});
        true ->
           Roster = lists:subtract(Meeting#uce_meeting.roster, [User]),
           update(Domain, Meeting#uce_meeting{roster=Roster})
    end.

roster(Domain, Id) ->
    {ok, Meeting} = get(Domain, Id),
    {ok, Meeting#uce_meeting.roster}.


% Live API

-spec publish(domain(), meeting(), #uce_event{}) -> ok.
publish(Domain, MeetingName, Event) ->
    EventManager = get_event_manager(Domain, MeetingName),
    gen_event:notify(EventManager, {event, Event}).


-spec subscribe(domain(), meeting(), pid()) -> ok.
subscribe(Domain, MeetingName, Subscriber) ->
    EventManager = get_event_manager(Domain, MeetingName),
    gen_event:add_handler(EventManager, {uce_subscription, Subscriber}, [Subscriber]).


-spec subscribe_meeting(domain(), meeting(), pid()) -> ok.
subscribe_meeting(Domain, MeetingName, Subscriber) ->
    EventManager = get_event_manager(Domain, MeetingName),
    gen_event:add_handler(EventManager, {uce_meeting_subscription, Subscriber}, [Subscriber]).

-spec unsubscribe(domain(), meeting(), pid()) -> ok.
unsubscribe(Domain, MeetingName, Subscriber) ->
    EventManager = get_event_manager(Domain, MeetingName),
    get_event_manager:delete_handler(EventManager, {uce_meeting_handler, Subscriber}, [Subscriber]).

-spec get_event_manager(domain(), meeting()) -> pid().
get_event_manager(Domain, MeetingName) ->
    [[Pid]] = ets:match(meeting_event_managers, {{Domain, MeetingName}, '$1'}),
    Pid.

