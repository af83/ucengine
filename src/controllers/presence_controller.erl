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

-export([init/0, delete/4, add/4, timeout/0, clean/0]).

-include("uce.hrl").
-include("uce_auth.hrl").

init() ->
    [#uce_route{method='POST',
                regexp="/presence",
                callbacks=[{?MODULE, add,
                            ["uid", "credential", "metadata"],
                            [none, none, []],
                            [string, string, dictionary]}]},
     
     #uce_route{method='DELETE',
                regexp="/presence/([^/]+)",
                callbacks=[{?MODULE, delete,
                            ["uid", "sid"],
                            [required, required],
                            [string, string]}]}].

add(Domain, [], [Name, Credential, Metadata], _) ->
    {ok, User} = uce_user:get({Name, Domain}),
    {ok, true} = uce_acl:assert(User#uce_user.id, "presence", "add"),
    {ok, true} = ?AUTH_MODULE(User#uce_user.auth):assert(User, Credential),
    {ok, Id} = uce_presence:add(#uce_presence{user=User#uce_user.id,
                                              domain=Domain,
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
    {ok, deleted} = uce_presence:delete(Record#uce_presence.id),

    lists:foreach(fun(Meeting) ->
                          catch uce_event:add(#uce_event{domain=Domain,
                                                         from=Record#uce_presence.user,
                                                         type="internal.roster.delete",
                                                         location=Meeting}),
                          catch uce_meeting:leave(Meeting, Record#uce_presence.user)
                  end,
                  Record#uce_presence.meetings),

    catch uce_event:add(#uce_event{domain=Domain,
                                   from=Record#uce_presence.user,
                                   type="internal.presence.delete"}),
    json_helpers:json(ok).

clean() ->
    ?MODULE:timeout(),
    case config:get(presence_timeout) of
        Value when is_integer(Value) ->
            timer:sleep(Value * 1000);
        _ ->
            timer:sleep(?DEFAULT_TIME_INTERVAL)
    end,
    ?MODULE:clean().

timeout() ->
    case uce_presence:all() of
        {ok, Presences} ->
            delete_expired_presence(Presences);
        _ ->
            nothing
    end.

delete_expired_presence([#uce_presence{id=Sid,
                                       user=Uid,
                                       last_activity=Last,
                                       auth=Auth,
                                       meetings=Meetings} | TlPresences]) ->
    ?DEBUG("Timeout: ~p~n", [?SESSION_TIMEOUT]),
    Timeout = Last + ?SESSION_TIMEOUT,
    Now = utils:now(),
    if
        Now >= Timeout , Auth == "password", Uid /= "root" ->
            F = fun(M) ->
                   uce_event:add(#uce_event{from=Uid,
                                            type="internal.roster.delete",
                                            location=[M]}),
                   uce_meeting:leave([M], Uid)
                end,
            [ F(Meeting) || Meeting <- Meetings ],
            uce_event:add(#uce_event{location=[""],
                                     type="internal.presence.delete",
                                     from=Uid}),
            uce_presence:delete(Sid);
        true ->
            nothing
    end,
    delete_expired_presence(TlPresences);

delete_expired_presence([]) ->
    ok.

