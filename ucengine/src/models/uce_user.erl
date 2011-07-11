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

-type rolelocation() :: {list(), list()}.

%
% Public api
%
-spec add(domain(), #uce_user{}) -> {ok, uid()} | erlang:throw({error, conflict}).
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

-spec delete(domain(), uid()) -> {ok, deleted} | erlang:throw({error, not_found}) | erlang:throw({error, any()}).
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
                            ok = uce_vhost_user_sup:terminate_child(Domain, Pid)
                    end,
                    (db:get(?MODULE, Domain)):delete(Domain, Uid)
            end;
        false ->
            throw({error, not_found})
    end.

-spec update(domain(), #uce_user{}) -> {ok, updated} | erlang:throw({error, not_found}).
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
            throw({error, not_found})
   end.

-spec list(domain()) -> {ok, list(#uce_user{})}.
list(Domain) ->
    (db:get(?MODULE, Domain)):list(Domain).

-spec get(domain(), uid()) -> {ok, #uce_user{}} | erlang:throw({error, not_found}).
get(Domain, Uid) ->
    case get_pid_of(Domain, Uid) of
        {error, not_found} ->
            (db:get(?MODULE, Domain)):get(Domain, Uid);
        {ok, Pid} ->
            gen_server:call(Pid, get_user)
    end.

-spec get_by_name(domain(), list()) -> {ok, #uce_user{}} | erlang:throw({error, not_found}).
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

-spec add_role(domain(), uid(), rolelocation()) -> {ok, updated} | erlang:throw({error, not_found}).
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
                    throw({error, not_found})
            end;
        false ->
            throw({error, not_found})
    end.

-spec delete_role(domain(), uid(), rolelocation()) -> {ok, updated} | erlang:throw({error, not_found}).
delete_role(Domain, Id, {Role, Location}) ->
    {ok, User} = get(Domain, Id),
    Roles = case lists:member({Role, Location}, User#uce_user.roles) of
                true ->
                    lists:delete({Role, Location}, User#uce_user.roles);
                false ->
                    throw({error, not_found})
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
    {ok, #state{domain=Domain, user=User}}.

handle_call(get_user, _From, #state{user=User} = State) ->
    {reply, {ok, User}, State};

handle_call({add_presence, #uce_presence{id=Sid}=Presence}, _From, #state{domain=Domain, presences=Presences} = State) ->
    gproc:add_local_name({Domain, sid, Sid}),
    {reply, ok, State#state{presences=[Presence|Presences]}};

handle_call({get_presence, Sid}, _From, #state{presences=Presences} = State) ->
    {reply, get_presence_by_sid(Sid, Presences), State};

handle_call(get_presences, _From, #state{presences=Presences} = State) ->
    {reply, Presences, State};

handle_call({update_presence, #uce_presence{id=Sid}=NewPresence}, _From, #state{presences=Presences} = State) ->
    {ok, Presence} = get_presence_by_sid(Sid, Presences),
    NewPresences = lists:delete(Presence, Presences),
    {reply, get_presence_by_sid(Sid, Presences), State#state{presences=[NewPresence|NewPresences]}};

handle_call({delete_presence, Sid}, _From, #state{presences=Presences} = State) ->
    {ok, Presence} = get_presence_by_sid(Sid, Presences),
    NewPresences = lists:delete(Presence, Presences),
    case NewPresences of
        [] ->
            {stop, "end of process", {ok, deleted}, State#state{presences=NewPresences}};
        _Other ->
            {reply, {ok, deleted}, State#state{presences=NewPresences}}
    end.

handle_cast({update_user, User}, State) ->
    {noreply, State#state{user=User}}.

handle_info(_Info, State) ->
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

-spec get_presence_by_sid(sid(), list(#uce_presence{})) -> {ok, #uce_presence{}} | {error, not_found}.
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
