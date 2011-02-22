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
-module(presence_controller).

-export([init/0, delete/4, add/4]).

-include("uce.hrl").
-include("uce_auth.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/presence",
                callbacks=[{?MODULE, add,
                            ["uid", "credential", "timeout", "metadata"],
                            [none, none, 0, []],
                            [string, string, integer ,dictionary]}]},

     #uce_route{method='DELETE',
                regexp="/presence/([^/]+)",
                callbacks=[{?MODULE, delete,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]}].

add(Domain, [], [Name, Credential, Timeout, Metadata], _) ->
    {ok, User} = uce_user:get({Name, Domain}),
    {ok, true} = uce_acl:assert(User#uce_user.id, "presence", "add"),
    {ok, true} = ?AUTH_MODULE(User#uce_user.auth):assert(User, Credential),
    {ok, Id} = uce_presence:add(#uce_presence{user=User#uce_user.id,
                                              domain=Domain,
                                              timeout=Timeout,
                                              auth=User#uce_user.auth,
                                              metadata=Metadata}),
    catch uce_event:add(#uce_event{domain=Domain,
                                   from=User#uce_user.id,
                                   location={"", Domain},
                                   type="internal.presence.add"}),
    json_helpers:created(Id).

delete(Domain, [Id], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, Record} = uce_presence:get(Id),
    {ok, true} = uce_acl:assert({Uid, Domain}, "presence", "delete", {"", Domain},
                                [{"id", Record#uce_presence.id}]),

    catch presence_helpers:clean(Record),

    {ok, deleted} = uce_presence:delete(Record#uce_presence.id),

    json_helpers:json(ok).
