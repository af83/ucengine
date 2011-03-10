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
    Domains = config:get(hosts),
    ?DEBUG("Domains : ~p~n", [Domains]),
    lists:foreach(fun({Domain, _Config}) ->
                          fill_domain(Domain)
                  end, Domains).

fill_domain(Domain) ->
    ?DEBUG("fill_domain Domain : ~p~n", [Domain]),
    uce_infos:update(Domain,
                     #uce_infos{domain=Domain,
                                metadata=[{"description", "U.C.Engine is a publish/subscribe server with persistence. It allows you to build real time applications like collaboration based services, live meetings, games or anything that fits well in an event driven philosophy."},
                                          {"logo", "ucengine.png"},
                                          {"htags", "ucengine"}]}),

    catch uce_meeting:add(Domain,
                          #uce_meeting{id={"demo", Domain},
                                       metadata=[{"description", "U.C.Engine demo meetup"}],
                                       start_date=utils:now(),
                                       end_date=?NEVER_ENDING_MEETING}),

    catch uce_meeting:add(Domain,
                          #uce_meeting{id={"demo2", Domain},
                                       metadata=[{"description", "Meeting R&D"},
                                                 {"video", "/test"}],
                                       start_date=utils:now(),
                                       end_date=?NEVER_ENDING_MEETING}),

    catch uce_meeting:add(Domain,
                          #uce_meeting{id={"agoroom", Domain},
                                       metadata=[{"description", "Meeting agoroom"},
                                                 {"video", "http://encre.2metz.fr/simonsinek_2009x"}],
                                       start_date=1287738533649,
                                       end_date=1287739733649}),

    Users = ["thierry.bomandouki@af83.com", "victor.goya@af83.com",
             "louis.ameline@af83.com", "alexandre.eisenchteter@af83.com",
             "romain.gauthier@af83.com", "participant"],

    lists:foreach(fun(User) ->
                          catch uce_user:add(Domain,
                                             #uce_user{id={User, Domain},
                                                       auth="password",
                                                       credential="pwd",
                                                       metadata=[]}),
                          uce_acl:add(Domain,
                                      #uce_acl{user={User, Domain},
                                               location={"", Domain},
                                               action="add",
                                               object="presence"})
                  end, Users),
                                                % anonymous account
    catch uce_user:add(Domain, #uce_user{id={"anonymous", Domain},
                                         auth="none"}),
    uce_acl:add(Domain, #uce_acl{user={"anonymous", Domain},
                                 action="add",
                                 object="presence"}),
    uce_acl:add(Domain, #uce_acl{user={"anonymous", Domain},
                                 action="get",
                                 object="infos"}),
    uce_acl:add(Domain, #uce_acl{user={"anonymous", Domain},
                                 action="get",
                                 object="meeting"}),
    uce_acl:add(Domain, #uce_acl{user={"anonymous", Domain},
                                 action="list",
                                 object="meeting"}),

    Hashtags = ["#TED", "#sinek", "#simonsinek", "#ucengine"],
    lists:foreach(fun(HashTag) ->
                          uce_event:add(Domain,
                                        #uce_event{domain=Domain,
                                                   type="twitter.hashtag.add",
                                                   location={"demo", Domain},
                                                   from={"participant", Domain},
                                                   metadata=[{"hashtag", HashTag}]})
                  end, Hashtags),
    ok.
