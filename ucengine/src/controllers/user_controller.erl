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

-export([init/0, add/5, update/5, get/5, find/5, list/5, delete/5, check_access/5, add_role/5, delete_role/5]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                path=["user"],
                middlewares=[{params, [{"uid", "", string},
                                       {"sid", "", string},
                                       {"name", required, string},
                                       {"auth", required, string},
                                       {"credential", required, string},
                                       {"metadata", [], dictionary}]}],
                callback={?MODULE, add}},

     #uce_route{method='GET',
                path=["user"],
                middlewares = [auth],
                callback={?MODULE, list}},

     #uce_route{method='GET',
                path=["user", id],
                middlewares = [auth],
                callback={?MODULE, get}},

     #uce_route{method='GET',
                path=["find","user"],
                middlewares = [auth,
                               {params, [{"by_name", "", string},
                                         {"by_uid", "", string}]}],
                callback={?MODULE, find}},

     #uce_route{method='PUT',
                path=["user", id],
                middlewares = [auth,
                               {params, [{"name", required, string},
                                         {"auth", required, string},
                                         {"credential", required, string},
                                         {"metadata", [], dictionary}]}],
                callback={?MODULE, update}},

     #uce_route{method='DELETE',
                path=["user", id],
                middlewares = [auth],
                callback={?MODULE, delete}},

     #uce_route{method='GET',
                path=["user", id, "can", action, object, '...'],
                middlewares = [auth,
                               {params, [{"conditions", [], dictionary}]}],
                callback={?MODULE, check_access}},

     #uce_route{method='POST',
                path=["user", id, "roles"],
                middlewares = [auth,
                               {params, [{"role", required, string},
                                         {"location", "", string}]}],
                callback={?MODULE, add_role}},

     #uce_route{method='DELETE',
                path=["user", id, "roles", role, '...'],
                middlewares = [auth],
                callback={?MODULE, delete_role}}].


add(Domain, [], [Uid, Sid, Name, Auth, Credential, Metadata], _Request, Response) ->
    case config:get(Domain, register) of
        open ->
            create_user(Response, Domain, [Name, Auth, Credential, Metadata]);
        restricted ->
            case uce_presence:assert(Domain, Uid, Sid) of
                ok ->
                    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "add"),
                    create_user(Response, Domain, [Name, Auth, Credential, Metadata]);
                unauthorized ->
                    throw({error, unauthorized})
            end
    end.

create_user(Response, Domain, [Name, Auth, Credential, Metadata]) ->
    {ok, NewUserUid} = uce_user:add(Domain, #uce_user{id=none,
                                                      name=Name,
                                                      auth=Auth,
                                                      credential=Credential,
                                                      metadata=json_helpers:to_struct(Metadata)}),

    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=NewUserUid,
                                               location="",
                                               type="internal.user.add"}),
    json_helpers:created(Response, NewUserUid).

list(Domain, [], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "list"),
    {ok, Users} = uce_user:list(Domain),
    json_helpers:json(Response, Domain, Users).

get(Domain, [{id, Id}], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "get", [{"user", Id}]),
    {ok, User} = uce_user:get(Domain, Id),
    json_helpers:json(Response, Domain, User).

find(Domain, [], [Uid, _Sid, ByName, _ByUid], _Request, Response) when ByName /= "" ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "get", []),
    {ok, User} = uce_user:get_by_name(Domain, ByName),
    json_helpers:json(Response, Domain, User);

find(Domain, [], [Uid, _Sid, _ByName, ByUid], _Request, Response) when ByUid /= "" ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "get", []),
    {ok, User} = uce_user:get(Domain, ByUid),
    json_helpers:json(Response, Domain, User);

find(_Domain, [], [_Uid, _Sid, _ByName, _ByUid], _Request, _Response)->
    throw({error, missing_parameter}).

update(Domain, [{id, Id}], [Uid, _Sid, Name, Auth, Credential, Metadata], _Request, Response) ->
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

    json_helpers:ok(Response).

delete(Domain, [{id, Id}], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "user", "delete", [{"user", Id}]),
    {ok, deleted} = uce_user:delete(Domain, Id),
    json_helpers:ok(Response).

check_access(Domain, [Name, Action, Object], [Uid, Sid, Conditions], Request, Response) ->
    check_access(Domain, [Name, Action, Object, ""], [Uid, Sid, Conditions], Request, Response);
check_access(Domain, [{id, Name}, {action, Action}, {object, Object}, Location], [Uid, _Sid, Conditions], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "access", "check", [{"user", Name},
                                                                        {"action", Action},
                                                                        {"object", Object},
                                                                        {"location", Location}]),
    case uce_access:check(Domain, Name, Location, Object, Action, Conditions) of
        {ok, true} ->
            json_helpers:true(Response);
        {ok, false} ->
            json_helpers:false(Response)
    end.

add_role(Domain, [{id, Name}], [Uid, _Sid, Role, Location], _Request, Response) ->
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
    json_helpers:ok(Response).

delete_role(Domain, [User, Role], [Uid, Sid], Request, Response) ->
    delete_role(Domain, [User, Role, ""], [Uid, Sid], Request, Response);
delete_role(Domain, [{id, User}, {role, Role}, Location], [Uid, _Sid], _Request, Response) ->
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
    json_helpers:ok(Response).
