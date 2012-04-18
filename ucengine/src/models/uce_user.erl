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
-module(uce_user).

% public api
-export([add/2,
         delete/2,
         update/2,
         list/1,
         get/2,
         get_by_name/2,
         exists/2,
         acl/3,
         add_role/3,
         delete_role/3,
         start_link/2]).

% gen_server callbacks
-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-behaviour(gen_server).

-include("uce.hrl").

%
% Public api
%
-spec add(domain(), user()) -> {ok, uid()} | erlang:throw({error, conflict}).
add(Domain, #uce_user{id=none} = User) ->
    add(Domain, User#uce_user{id=utils:random()});
add(Domain, #uce_user{id=UId, name=Name} = User) ->
    case exists_by_name(Domain, Name) of
        true ->
            throw({error,conflict});
        false ->
            uce_role:add(Domain, #uce_role{id=UId}),
            DefaultRoles = [{"default", ""}, {UId, ""}],
            (db:get(?MODULE, Domain)):add(Domain,
                                          User#uce_user{roles=User#uce_user.roles ++ DefaultRoles}),
            {ok, UId}
    end.

-spec delete(domain(), uid()) -> {ok, deleted} | erlang:throw({error, not_found, string()}) | erlang:throw({error, any()}).
delete(Domain, Uid) ->
    case exists(Domain, Uid) of
        true ->
            % delete the default role
            case catch uce_role:delete(Domain, Uid) of
                {error, Reason} when Reason /= not_found ->
                    throw({error, Reason});
                {ok, deleted}->
                    case get_pid_of(Domain, Uid) of
                        {error, not_found} ->
                            ok;
                        {ok, Pid} ->
                            ok = gen_server:call(Pid, stop)
                    end,
                    (db:get(?MODULE, Domain)):delete(Domain, Uid)
            end;
        false ->
            throw({error, not_found, "user not found"})
    end.

-spec update(domain(), user()) -> {ok, updated} | erlang:throw({error, not_found, string()}).
update(Domain, #uce_user{id=Uid} = User) ->
    case exists(Domain, Uid) of
        true ->
            case get_pid_of(Domain, Uid) of
                {error, not_found} ->
                    ok;
                {ok, Pid} ->
                    gen_server:cast(Pid, {update_user, User})
            end,
            (db:get(?MODULE, Domain)):update(Domain, User);
        false ->
            throw({error, not_found, "user not found"})
   end.

-spec list(domain()) -> {ok, list(user())}.
list(Domain) ->
    (db:get(?MODULE, Domain)):list(Domain).

-spec get(domain(), uid()) -> {ok, user()} | erlang:throw({error, not_found}).
get(Domain, Uid) ->
    case get_pid_of(Domain, Uid) of
        {error, not_found} ->
            (db:get(?MODULE, Domain)):get(Domain, Uid);
        {ok, Pid} ->
            gen_server:call(Pid, get_user)
    end.

-spec get_by_name(domain(), list()) -> {ok, user()} | erlang:throw({error, not_found, string()}).
get_by_name(Domain, Name) ->
    (db:get(?MODULE, Domain)):get_by_name(Domain, Name).

-spec exists(domain(), uid()) -> true | false | erlang:throw({error, any()}).
% "" value are used in uce_event:add
% From or To can be empty
exists(_Domain, "") ->
    true;
exists(Domain, Uid) ->
    case get_pid_of(Domain, Uid) of
        {error, not_found} ->
            case catch get(Domain, Uid) of
                {error, not_found} ->
                    false;
                {error, Reason} ->
                    throw({error, Reason});
                {ok, _User}->
                    true
            end;
        {ok, _Pid} ->
            true
    end.

-spec add_role(domain(), uid(), {role_id(), meeting_id()}) -> {ok, updated} | erlang:throw({error, not_found, string()}).
add_role(Domain, Uid, {Role, Location}) ->
    % Just ensure the role and location exists
    case uce_meeting:exists(Domain, Location) of
        true ->
            case uce_role:exists(Domain, Role) of
                true ->
                    {ok, User} = get(Domain, Uid),
                    case lists:member({Role, Location}, User#uce_user.roles) of
                        true ->
                            {ok, updated};
                        false ->
                            update(Domain, User#uce_user{roles=(User#uce_user.roles ++ [{Role, Location}])})
                    end;
                false ->
                    throw({error, not_found, "role not found"})
            end;
        false ->
            throw({error, not_found, "meeting not found"})
    end.

-spec delete_role(domain(), uid(), {role_id(), meeting_id()}) -> {ok, updated} | erlang:throw({error, not_found, string()}).
delete_role(Domain, Id, {Role, Location}) ->
    {ok, User} = get(Domain, Id),
    Roles = case lists:member({Role, Location}, User#uce_user.roles) of
                true ->
                    lists:delete({Role, Location}, User#uce_user.roles);
                false ->
                    throw({error, not_found, "role not found for this user"})
            end,
    update(Domain, User#uce_user{roles=Roles}).

-spec acl(domain(), uid(), list()) -> {ok, list()}.
acl(Domain, User, Location) ->
    {ok, Record} = get(Domain, User),
    ACL = lists:map(fun({RoleName, RoleLocation}) ->
                            {ok, RoleACL} =
                                if
                                    RoleLocation == "" ->
                                        uce_role:acl(Domain, RoleName);
                                    RoleLocation == Location ->
                                        uce_role:acl(Domain, RoleName);
                                    true ->
                                        {ok, []}
                                end,
                            RoleACL
                    end,
                    Record#uce_user.roles),
    {ok, lists:flatten(ACL)}.

%
% gen server callbacks
%

-record(state, {
          domain,
          user,
          presences = []
         }).

init([Domain, #uce_user{id=Uid} = User]) ->
    gproc:add_local_name({Domain, uid, Uid}),
    timer:send_after(config:get(timeout_refresh) * 1000, check_timeout),
    {ok, #state{domain=Domain, user=User}}.

handle_call(get_user, _From, #state{user=User} = State) ->
    {reply, {ok, User}, State};

handle_call({add_presence, #uce_presence{id=Sid}=Presence}, _From, #state{domain=Domain, presences=Presences} = State) ->
    gproc:add_local_name({Domain, sid, Sid}),
    {reply, ok, State#state{presences=[Presence|Presences]}};

handle_call({get_presence, Sid}, _From, #state{presences=Presences} = State) ->
    {reply, get_presence_by_sid(Sid, Presences), State};

handle_call({update_presence, #uce_presence{id=Sid}=NewPresence}, _From, #state{presences=Presences} = State) ->
    {Presence, NewPresences} = delete_presence_from_sid(Sid, Presences),
    {reply, {ok, Presence}, State#state{presences=[NewPresence|NewPresences]}};

%%
%% supervisor:terminate_child doesn't work with simple_one_for_one in erlang < R14BO3
%%
handle_call(stop, _From, State) ->
    {stop, "normal", ok, State}.

handle_cast({update_user, User}, State) ->
    {noreply, State#state{user=User}};

handle_cast({add_stream, Sid}, #state{presences=Presences} = State) ->
    {Presence, NewPresences} = delete_presence_from_sid(Sid, Presences),
    NbStream = Presence#uce_presence.streams + 1,
    {noreply, State#state{presences=[Presence#uce_presence{streams=NbStream}|NewPresences]}};

handle_cast({remove_stream, Sid}, #state{presences=Presences} = State) ->
    {Presence, NewPresences} = delete_presence_from_sid(Sid, Presences),
    NbStream = Presence#uce_presence.streams - 1,
    {noreply, State#state{presences=[Presence#uce_presence{streams=NbStream,
                                                           last_activity=utils:now()}|NewPresences]}};

handle_cast({delete_presence, Sid}, #state{domain=Domain, user=User, presences=Presences} = State) ->
    {PresenceToDelete, NewPresences} = delete_presence_from_sid(Sid, Presences),
    ok = disconnect_from_meetings(Domain, User, PresenceToDelete, NewPresences),
    case NewPresences of
        [] ->
            {stop, normal, State#state{presences=NewPresences}};
        _Other ->
            {noreply, State#state{presences=NewPresences}}
    end.

handle_info(check_timeout, #state{domain=Domain,
                                  presences=Presences} = State) ->
    cleanup_presence(Domain, Presences, utils:now()),
    timer:send_after(config:get(timeout_refresh) * 1000, check_timeout),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%
% Private function
%

start_link(Domain, User) ->
    gen_server:start_link(?MODULE, [Domain, User], []).

-spec exists_by_name(domain(), list()) -> true | false | erlang:throw({error, any()}).
exists_by_name(Domain, Name) ->
    case catch get_by_name(Domain, Name) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _User}->
            true
    end.

-spec get_presence_by_sid(sid(), list(presence())) -> {ok, presence()} | {error, not_found}.
get_presence_by_sid(_Sid, []) ->
    {error, not_found};
get_presence_by_sid(Sid, [#uce_presence{id=Sid} = Presence|_Presences]) ->
    {ok, Presence};
get_presence_by_sid(Sid, [_Presence|Presences]) ->
    get_presence_by_sid(Sid, Presences).

get_pid_of(Domain, Uid) ->
    case gproc:lookup_local_name({Domain, uid, Uid}) of
        undefined ->
            {error, not_found};
        Pid ->
            {ok, Pid}
    end.

%
% Cleanup old presence
%
cleanup_presence(Domain, [#uce_presence{id=Sid,
                                        streams=0,
                                        last_activity=LastActivity,
                                        timeout=Timeout}|Rest], Now) ->
    if
        LastActivity + (Timeout * 1000) < Now ->
            {ok, deleted} = uce_presence:delete(Domain, Sid),
            ?COUNTER(timeout);
        true ->
            nothing
    end,
    cleanup_presence(Domain, Rest, Now);
cleanup_presence(Domain, [_Presence|Rest], Now) ->
    cleanup_presence(Domain, Rest, Now);
cleanup_presence(_Domain, [], _Now) ->
    ok.


%
% Return all presence associated to the user
%
-spec get_all_meetings_of_user(list(presence()), uid()) -> list(meeting_id()) | [].
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
disconnect_from_meetings(Domain, #uce_user{id=Uid}, #uce_presence{meetings=Meetings}, Presences) ->
    UserMeetings = sets:to_list(sets:subtract(sets:from_list(Meetings), sets:from_list(get_all_meetings_of_user(Presences)))),
    clean_meetings(Domain, Uid, UserMeetings).

clean_meetings(Domain, Uid, Meetings) ->
    clean_meeting(Domain, Meetings, Uid),
    uce_event:add(Domain, #uce_event{id=none,
                                     from=Uid,
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

delete_presence_from_sid(Sid, Presences) ->
    {ok, Presence} = get_presence_by_sid(Sid, Presences),
    NewPresences = lists:delete(Presence, Presences),
    {Presence, NewPresences}.
