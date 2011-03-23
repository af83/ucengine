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

-author('tbomandouki@af83.com').

-export([add/2, delete/2, update/2, list/1, get/2, exists/2, acl/3, addRole/3, deleteRole/3]).

-include("uce.hrl").

add(Domain, #uce_user{id={_Name,Domain}=Id} = User) ->
    case exists(Domain, Id) of
        true ->
            throw({error, conflict});
        false ->
            apply(db:get(?MODULE, Domain), add, [Domain, User])
    end.

delete(Domain, Id) ->
    case exists(Domain, Id) of
        true ->
            apply(db:get(?MODULE, Domain), delete, [Domain, Id]);
        false ->
            throw({error, not_found})
    end.

update(Domain, #uce_user{id={_Name, Domain}=Id} = User) ->
    case ?MODULE:exists(Domain, Id) of
        true ->
            apply(db:get(?MODULE, Domain), update, [Domain, User]);
        false ->
            throw({error, not_found})
    end.

list(Domain) ->
    apply(db:get(?MODULE, Domain), list, [Domain]).

get(Domain, User) ->
    apply(db:get(?MODULE, Domain), get, [Domain, User]).

exists(Domain, Id) ->
    case Id of
        {"", _} -> % all
            true;
        _ ->
            case catch ?MODULE:get(Domain, Id) of
                {error, not_found} ->
                    false;
                {error, Reason} ->
                    throw({error, Reason});
                _ ->
                    true
            end
    end.

addRole(Domain, Id, {Role, Location}) ->
    % Just ensure the role and location exists
    case uce_meeting:exists(Domain, {Location, Domain}) of
        true ->
            case uce_role:exists(Domain, {Role, Domain}) of
                true ->
                    {ok, User} = ?MODULE:get(Domain, Id),
                    case lists:member({Role, Location}, User#uce_user.roles) of
                        true ->
                            {ok, updated};
                        false ->
                            ?MODULE:update(Domain, User#uce_user{roles=(User#uce_user.roles ++ [{Role, Location}])})
                    end;
                false ->
                    throw({error, not_found})
            end;
        false ->
            throw({error, not_found})
    end.

deleteRole(Domain, Id, {Role, Location}) ->
    {ok, User} = ?MODULE:get(Domain, Id),
    Roles = case lists:member({Role, Location}, User#uce_user.roles) of
                true ->
                    lists:delete({Role, Location}, User#uce_user.roles);
                false ->
                    throw({error, not_found})
            end,
    ?MODULE:update(Domain, User#uce_user{roles=Roles}).

acl(Domain, User, {Location, _}) ->
    {ok, Record} = ?MODULE:get(Domain, User),
    ACL = lists:map(fun({RoleName, RoleLocation}) ->
                            {ok, RoleACL} =
                                if
                                    RoleLocation == "" ->
                                        uce_role:acl(Domain, {RoleName, Domain});
                                    RoleLocation == Location ->
                                        uce_role:acl(Domain, {RoleName, Domain});
                                    true ->
                                        {ok, []}
                                end,
                            RoleACL
                    end,
                    Record#uce_user.roles),
    {ok, lists:flatten(ACL)}.
