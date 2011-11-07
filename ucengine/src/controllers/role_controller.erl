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
-module(role_controller).

-export([init/0, add/5, delete/5, add_access/5, delete_access/5]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                path=["role"],
                middlewares = [auth,
                               {params, [{"name", required, string}]}],
                callback={?MODULE, add}},

     #uce_route{method='DELETE',
                path=["role", name],
                middlewares = [auth],
                callback={?MODULE, delete}},

     #uce_route{method='POST',
                path=["role", name, "acl"],
                middlewares = [auth,
                               {params, [{"object", "all", string},
                                         {"action", "all", string},
                                         {"conditions", [], dictionary}]}],
                callback={?MODULE, add_access}},

     #uce_route{method='DELETE',
                path=["role", name, "acl", object, action],
                middlewares = [auth,
                               {params, [{"conditions", [], dictionary}]}],
                callback={?MODULE, delete_access}}].

add(Domain, [], [Uid, _Sid, Name], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "role", "add", [{"name", Name}]),
    {ok, created} = uce_role:add(Domain, #uce_role{id=Name}),
    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.role.add",
                                               metadata=[{"name", Name}]}),

    json_helpers:created(Response).

delete(Domain, [{name, Name}], [Uid, _Sid], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "", "role", "delete", [{"name", Name}]),
    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.role.delete",
                                               metadata=[{"name", Name}]}),
    {ok, deleted} = uce_role:delete(Domain, Name),
    json_helpers:ok(Response).

add_access(Domain, [{name, Role}], [Uid, _Sid, Object, Action, Conditions], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "",
                                   "access", "add", [{"role", Role},
                                                     {"object", Object},
                                                     {"action", Action}]),

    {ok, updated} = uce_role:add_access(Domain, Role,
                                       #uce_access{object=Object,
                                                   action=Action,
                                                   conditions=Conditions}),

    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.access.add",
                                               metadata=[{"role", Role},
                                                         {"action", Action},
                                                         {"object", Object}] ++
                                                   Conditions}),

    json_helpers:ok(Response).

delete_access(Domain, [{name, Role}, {object, Object}, {action, Action}], [Uid, _Sid, Conditions], _Request, Response) ->
    {ok, true} = uce_access:assert(Domain, Uid, "",
                                   "access", "add", [{"role", Role},
                                                     {"object", Object},
                                                     {"action", Action}]),

    {ok, updated} = uce_role:delete_access(Domain, Role,
                                          #uce_access{object=Object,
                                                      action=Action,
                                                      conditions=Conditions}),

    {ok, _} = uce_event:add(Domain, #uce_event{id=none,
                                               from=Uid,
                                               location="",
                                               type="internal.access.delete",
                                               metadata=[{"role", Role},
                                                         {"action", Action},
                                                         {"object", Object}] ++
                                                   Conditions}),

    json_helpers:ok(Response).
