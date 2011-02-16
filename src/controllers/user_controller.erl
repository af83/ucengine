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
-module(user_controller).

-export([init/0, add/4, update/4, get/4, list/4, delete/4]).

-include("uce.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/user",
                callbacks=[{?MODULE, add,
                            ["uid", "auth", "credential", "metadata"],
                            [required, required, required, []],
                            [string, string, string, dictionary]}]},
     
     #uce_route{method='GET',
                regexp="/user",
                callbacks=[{?MODULE, list,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},

     #uce_route{method='GET',
                regexp="/user/([^/]+)",
                callbacks=[{?MODULE, get,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]},
     
     #uce_route{method='PUT',
                regexp="/user/([^/]+)",
                callbacks=[{?MODULE, update,
                            ["uid", "sid", "auth", "credential", "metadata"],
                            [required, required, required, required, []],
                            [string, string, string, string, dictionary]}]},
     
     #uce_route{method='DELETE',
                regexp="/user/([^/]+)",
                callbacks=[{?MODULE, delete,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]}].


add(Domain, [], [Name, Auth, Credential, Metadata], _) ->
    {ok, created} = uce_user:add(#uce_user{id={Name, Domain},
                                           auth=Auth,
                                           credential=Credential,
                                           metadata=Metadata}),

    catch uce_event:add(#uce_event{domain=Domain,
                                   from={Name, Domain},
                                   location={"", Domain},
                                   type="internal.user.add"}),

    json_helpers:created().

list(Domain, [], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "user", "list"),
    {ok, Users} = uce_user:list(Domain),
    json_helpers:json({array, [user_helpers:to_json(User) || User <- Users]}).

get(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "user", "get", {"", Domain}, [{"user", Name}]),
    {ok, Record} = uce_user:get({Name, Domain}),
    json_helpers:json(user_helpers:to_json(Record)).

update(Domain, [Name], [Uid, Sid, Auth, Credential, Metadata], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "user", "update", {"", Domain}, [{"user", Name},
                                                                                {"auth", Auth}]),
    {ok, updated} = uce_user:update(#uce_user{id={Name, Domain},
                                              auth=Auth,
                                              credential=Credential,
                                              metadata=Metadata}),

    catch uce_event:add(#uce_event{domain=Domain,
                                   from={Name, Domain},
                                   location={"", Domain},
                                   type="internal.user.update"}),
    
    json_helpers:ok().

delete(Domain, [Name], [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),
    {ok, true} = uce_acl:assert({Uid, Domain}, "user", "delete", {"", Domain}, [{"user", Name}]),
    {ok, deleted} = uce_user:delete({Name, Domain}),
    json_helpers:ok().
