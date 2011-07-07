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
-module(uce_timeout).

-behaviour(gen_server).

-include("uce.hrl").

-export([start_link/1, clean_meetings/3, force/1]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%
% Public API
%

start_link(Domain) ->
    gen_server:start_link({local, uce_vhost_sup:name(Domain, "timeout")}, ?MODULE, [Domain], []).

force(Domain) ->
    gen_server:call(uce_vhost_sup:name(Domain, "timeout"), run).

%
% gen_server callbacks
%

init([Domain]) ->
    gen_server:cast(self(), run),
    {ok, Domain}.

handle_call(run , _, Domain) ->
    clean(Domain),
    {reply, ok, Domain}.

%%
%% Cleanup old session in database and kick user from meeting room
%%
handle_cast(run, Domain) ->
    timer:sleep(config:get(timeout_refresh) * 1000),
    clean(Domain),
    handle_cast(run, Domain),
    {noreply, Domain};

handle_cast(_, State) ->
    {noreply, State}.

code_change(_,State,_) ->
    {ok, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Reason, _State) ->
    ok.

%
% Private functions
%

clean(Domain) ->
    {ok, Pids} = uce_presence:all(Domain),
    % delete expired presences
    Now = utils:now(),
    cleanup_presence(Domain, Pids, Now).

%
% Cleanup old presence
%
cleanup_presence(Domain, [Pid|Rest], Now) when is_pid(Pid) ->
    Presences = uce_presence:get_all(Domain, Pid),
    cleanup_presence(Domain, Presences, Now),
    cleanup_presence(Domain, Rest, Now);

cleanup_presence(Domain, [#uce_presence{id=Sid, user=Uid,
                                        last_activity=LastActivity,
                                        timeout=Timeout,
                                        meetings=Meetings}|Rest], Now) ->
    if
        LastActivity + (Timeout * 1000) < Now ->
            UserMeetings = diff(Meetings, get_all_meetings_of_user(Rest)),
            ok = clean_meetings(Domain, Uid, UserMeetings),
            {ok, deleted} = uce_presence:delete(Domain, Sid),
            ?COUNTER(timeout);
        true ->
            nothing
    end,
    cleanup_presence(Domain, Rest, Now);
cleanup_presence(_Domain, [], _Now) ->
    ok.

diff(L1, L2) ->
    lists:filter(fun(X) -> not lists:member(X, L2) end, L1).

-spec get_all_meetings_of_user(list(#uce_presence{}), uid())
                              -> list(meeting()) | list().
%
% Return all presence associated to the user
%
get_all_meetings_of_user(Presences) ->
    get_all_meetings_of_user(Presences, []).

get_all_meetings_of_user([#uce_presence{meetings=Meetings}|Rest], Result) ->
    get_all_meetings_of_user(Rest, Meetings ++ Result);
% the end
get_all_meetings_of_user([], Result) ->
    Result.
%
% Cleanup presence in meetings
%
clean_meetings(Domain, User, Meetings) ->
    clean_meeting(Domain, Meetings, User),
    uce_event:add(Domain, #uce_event{id=none,
                                     from=User,
                                     type="internal.presence.delete",
                                     location=""}),
    ok.

clean_meeting(_Domain, [], _Uid) ->
    ok;
clean_meeting(Domain, [Meeting|Meetings], Uid) ->
    try uce_event:add(Domain, #uce_event{id=none,
                                         from=Uid,
                                         type="internal.roster.delete",
                                         location=Meeting}) of
        {ok, _Id} ->
            try uce_meeting:leave(Domain, Meeting, Uid) of
                {ok, updated} ->
                    ok
            catch
                {error, Reason} ->
                    ?ERROR_MSG("Error when cleanup meeting presence of ~p : ~p", [Uid, Reason])
            end
    catch
        {error, Reason} ->
            ?ERROR_MSG("Error when cleanup roster presence of ~p : ~p", [Uid, Reason])
    end,
    clean_meeting(Domain, Meetings, Uid).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

diff_test() ->
    ?assertEqual(["meeting1", "meeting2"], diff(["meeting1", "meeting2", "meeting3"], ["meeting3", "meeting4"])).

-endif.
