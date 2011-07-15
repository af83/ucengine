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
-module(user_controller).

-export([init/0, add/4, update/4, get/4, list/4, delete/4, check_access/4, add_role/4, delete_role/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                path=["user"],
                callback={?MODULE, add,
                          [{"name", required, string},
                           {"auth", required, string},
                           {"credential", required, string},
                           {"metadata", [], dictionary}]}},

     #uce_route{method='GET',
                path=["user"],
                callback={?MODULE, list,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='GET',
                path=["user", id],
                callback={?MODULE, get,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='PUT',
                path=["user", id],
                callback={?MODULE, update,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"name", required, string},
                           {"auth", required, string},
                           {"credential", required, string},
                           {"metadata", [], dictionary}]}},

     #uce_route{method='DELETE',
                path=["user", id],
                callback={?MODULE, delete,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='GET',
                path=["user", id, "can", action, object, '...'],
                callback={?MODULE, check_access,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"conditions", [], dictionary}]}},

     #uce_route{method='POST',
                path=["user", id, "roles"],
                callback={?MODULE, add_role,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"role", required, string},
                           {"location", "", string}]}},

     #uce_route{method='DELETE',
                path=["user", id, "roles", role, '...'],
                callback={?MODULE, delete_role,
                          [{"uid", required, string},
                           {"sid", required, string}]}}].


add(Domain, [], [Name, Auth, Credential, Metadata], _) ->
    {ok, UId} = uce_user:add(Domain, #uce_user{id=none,
                                               name=Name,
                                               auth=Auth,
                                               credential=Credential,
                                               metadata=json_helpers:to_struct(Metadata)}),

    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=UId,
                                               location="",
                                               type="internal.user.add"}),

    json_helpers:created(Domain, UId).

list(Domain, [], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "list"),
    {ok, Users} = uce_user:list(Domain),
    json_helpers:json(Domain, Users).

get(Domain, [{id, Id}], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "get", [{"user", Id}]),
    {ok, User} = uce_user:get(Domain, Id),
    json_helpers:json(Domain, User).

update(Domain, [{id, Id}], [Uid, Sid, Name, Auth, Credential, Metadata], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "update", [{"user", Id},
                                                                       {"auth", Auth}]),
    {ok, Record} = uce_user:get(Domain, Id),
    {ok, updated} = uce_user:update(Domain, Record#uce_user{name=Name,
                                                            auth=Auth,
                                                            credential=Credential,
                                                            metadata=json_helpers:to_struct(Metadata)}),

    {ok, _} = uce_event:add(Domain,
                            #uce_event{id=none,
                                       from=Id,
                                       location="",
                                       type="internal.user.update"}),

    json_helpers:ok(Domain).

delete(Domain, [{id, Id}], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "delete", [{"user", Id}]),
    {ok, deleted} = uce_user:delete(Domain, Id),
    json_helpers:ok(Domain).

check_access(Domain, [Name, Action, Object], [Uid, Sid, Conditions], Arg) ->
    check_access(Domain, [Name, Action, Object, ""], [Uid, Sid, Conditions], Arg);
check_access(Domain, [{id, Name}, {action, Action}, {object, Object}, Location], [Uid, Sid, Conditions], _Arg) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, "", "access", "check", [{"user", Name},
                                                                        {"action", Action},
                                                                        {"object", Object},
                                                                        {"location", Location}]),
    case uce_access:check(Domain, Name, Location, Object, Action, Conditions) of
        {ok, true} ->
            json_helpers:true(Domain);
        {ok, false} ->
            json_helpers:false(Domain)
    end.

add_role(Domain, [{id, Name}], [Uid, Sid, Role, Location], _) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, Location, "user.role", "add", [{"user", Name},
                                                                               {"role", Role}]),
    {ok, updated} = uce_user:add_role(Domain, Name, {Role, Location}),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id=none,
                                       from=Uid,
                                       location=Location,
                                       type="internal.user.role.add",
                                       metadata=[{"role", Role},
                                                 {"user", Name}]}),
    json_helpers:ok(Domain).

delete_role(Domain, [User, Role], [Uid, Sid], Arg) ->
    delete_role(Domain, [User, Role, ""], [Uid, Sid], Arg);
delete_role(Domain, [{id, User}, {role, Role}, Location], [Uid, Sid], _Arg) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),
    {ok, true} = uce_access:assert(Domain, Uid, Location, "user.role", "delete", [{"user", User},
                                                                                  {"role", Role}]),
    {ok, updated} = uce_user:delete_role(Domain, User, {Role, Location}),
    {ok, _} = uce_event:add(Domain,
                            #uce_event{id=none,
                                       from=Uid,
                                       location=Location,
                                       type="internal.user.role.delete",
                                       metadata=[{"role", Role},
                                                 {"user", User}]}),
    json_helpers:ok(Domain).
