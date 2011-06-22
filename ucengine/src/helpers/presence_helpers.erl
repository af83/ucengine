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
-module(presence_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([clean/2, to_json/2]).

to_json(Domain, #uce_presence{id=Id,
                              user=User,
                              auth=Auth,
                              metadata=Metadata}) ->
    {struct,
     [{id, Id},
      {domain, Domain},
      {user, User},
      {auth, Auth},
      {metadata, {struct, Metadata}}]}.

clean(Domain, #uce_presence{
                    user=User,
                    meetings=Meetings}) ->
    lists:foreach(fun(Meeting) ->
                          uce_event:add(Domain, #uce_event{id={none, Domain},
                                                           from={User, Domain},
                                                           type="internal.roster.delete",
                                                           location={Meeting, Domain}}),
                          uce_meeting:leave(Domain, Meeting, User)
                  end,
                  Meetings),
    uce_event:add(Domain, #uce_event{id={none, Domain},
                                     from={User, Domain},
                                     type="internal.presence.delete",
                                     location={"", Domain}}),
    ok.
