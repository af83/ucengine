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
         delete_role/3]).

-include("uce.hrl").

%
% Public api
%
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

delete(Domain, Uid) ->
    case exists(Domain, Uid) of
        true ->
            % delete the default role
            case catch uce_role:delete(Domain, Uid) of
                {error, Reason} when Reason /= not_found ->
                    throw({error, Reason});
                {ok, deleted}->
                    (db:get(?MODULE, Domain)):delete(Domain, Uid)
            end;
        false ->
            throw({error, not_found})
    end.

update(Domain, #uce_user{id=Uid} = User) ->
    case exists(Domain, Uid) of
        true ->
            (db:get(?MODULE, Domain)):update(Domain, User);
        false ->
            throw({error, not_found})
   end.

list(Domain) ->
    (db:get(?MODULE, Domain)):list(Domain).

get(Domain, User) ->
    (db:get(?MODULE, Domain)):get(Domain, User).

get_by_name(Domain, Name) ->
    (db:get(?MODULE, Domain)):get_by_name(Domain, Name).

% "" value are used in uce_event:add
% From or To can be empty
exists(_Domain, "") ->
    true;
exists(Domain, Id) ->
    case catch get(Domain, Id) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _User}->
            true
    end.

exists_by_name(Domain, Name) ->
    case catch get_by_name(Domain, Name) of
        {error, not_found} ->
            false;
        {error, Reason} ->
            throw({error, Reason});
        {ok, _User}->
            true
    end.

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

delete_role(Domain, Id, {Role, Location}) ->
    {ok, User} = get(Domain, Id),
    Roles = case lists:member({Role, Location}, User#uce_user.roles) of
                true ->
                    lists:delete({Role, Location}, User#uce_user.roles);
                false ->
                    throw({error, not_found})
            end,
    update(Domain, User#uce_user{roles=Roles}).

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
