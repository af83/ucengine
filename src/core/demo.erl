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
-module(demo).

-export([start/0]).

-include("uce.hrl").


start() ->

    Domain = config:get(default_domain),

    uce_infos:update(#uce_infos{domain=Domain,
                                metadata=[{"description", "U.C.Engine is a publish/subscribe server with persistence. It allows you to build real time applications like collaboration based services, live meetings, games or anything that fits well in an event driven philosophy."},
                                         {"logo", "ucengine.png"},
                                         {"htags", "ucengine"}]}),

    catch uce_meeting:add(#uce_meeting{id={"demo", Domain},
                                       metadata=[{"description", "U.C.Engine demo meetup"}],
                                       start_date=utils:now(),
                                       end_date=?NEVER_ENDING_MEETING}),

    catch uce_user:add(#uce_user{id={"thierry.bomandouki@af83.com", Domain},
                                 auth="password",
                                 credential="pwd",
                                 metadata=[]}),
    uce_acl:add(#uce_acl{user={"thierry.bomandouki@af83.com", Domain},
                         action="add",
                         object="presence"}),

    catch uce_user:add(#uce_user{id={"victor.goya@af83.com", Domain},
                                 auth="password",
                                 credential="pwd",
                                 metadata=[]}),
    uce_acl:add(#uce_acl{user={"victor.goya@af83.com", Domain},
                         action="add",
                         object="presence"}),

    catch uce_user:add(#uce_user{id={"louis.ameline@af83.com", Domain},
                                 auth="password",
                                 credential="pwd",
                                 metadata=[]}),
    uce_acl:add(#uce_acl{user={"louis.ameline@af83.com", Domain},
                         action="add",
                         object="presence"}),

    catch uce_user:add(#uce_user{id={"alexandre.eisenchteter@af83.com", Domain},
                                 auth="password",
                                 credential="pwd",
                                 metadata=[]}),
    uce_acl:add(#uce_acl{user={"alexandre.eisenchteter@af83.com", Domain},
                         action="add",
                         object="presence"}),
    
    catch uce_user:add(#uce_user{id={"romain.gauthier@af83.com", Domain},
                                 auth="password",
                                 credential="pwd",
                                 metadata=[]}),
    uce_acl:add(#uce_acl{user={"romain.gauthier@af83.com", Domain},
                         action="add",
                         object="presence"}),

    catch uce_user:add(#uce_user{id={"participant", Domain},
                                 auth="password",
                                 credential="pwd",
                                 metadata=[]}),
    uce_acl:add(#uce_acl{user={"participant", Domain},
                         action="add",
                         object="presence"}),
    
    uce_event:add(#uce_event{domain=Domain,
                             type="twitter.hashtag.add",
                             location={"demo", Domain},
                             from={"participant", Domain},
                             metadata=[{"hashtag", "#TED"}]}),
    uce_event:add(#uce_event{domain=Domain,
                             type="twitter.hashtag.add",
                             location={"demo", Domain},
                             from={"participant", Domain},
                             metadata=[{"hashtag", "#sinek"}]}),
    uce_event:add(#uce_event{domain=Domain,
                             type="twitter.hashtag.add",
                             location={"demo", Domain},
                             from={"participant", Domain},
                             metadata=[{"hashtag", "#simonsinek"}]}),
    uce_event:add(#uce_event{domain=Domain,
                             type="twitter.hashtag.add",
                             location={"demo", Domain},
                             from={"participant", Domain},
                             metadata=[{"hashtag", "#ucengine"}]}),

    catch uce_meeting:add(#uce_meeting{id={"demo2", Domain},
                                       metadata=[{"description", "Meeting R&D"},
                                                 {"video", "/test"}],
                                       start_date=utils:now(),
                                       end_date=?NEVER_ENDING_MEETING}),
    catch uce_meeting:add(#uce_meeting{id={"agoroom", Domain},
                                       metadata=[{"description", "Meeting agoroom"},
                                                 {"video", "http://encre.2metz.fr/simonsinek_2009x"}],
                                       start_date=1287738533649,
                                       end_date=1287739733649}),
    

%    ok = feed(Domain),

    case utils:get(config:get(admin), [uid, auth]) of
        [Uid, Auth] ->
            {ok, Sid} = uce_presence:add(#uce_presence{user={Uid, Domain},
                                                       auth=Auth,
                                                       metadata=[]}),
            io:format("Admin: ~p/~p~n", [Uid, Sid]);
        Reason ->
            io:format("No admin account (~p)~n", [Reason])
    end,
    ok.


%% feed(_, []) ->
%%     ok;
%% feed(Domain, [Path|Paths]) ->
%%     ["config", "samples", Meeting, _File] = re:split(Path, "/", [{return, list}]),
%%     event_helpers:feed(Domain, Path, [{"location", [Meeting]}]),
%%     feed(Domain, Paths).

%% feed(Domain) ->
%%     Paths = filelib:wildcard("config/samples/*/*.json"),
%%     feed(Domain, Paths).
