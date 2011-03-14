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
-module(solr_tests).

-include("uce.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([test_add/1, test_search/1]).

solr_test_() ->
    { setup
    , fun fixtures:setup/0
    , fun fixtures:teardown/1
    , fun([Domain, _, _]) ->
        [ ?_test(test_add(Domain))
        , ?_test(test_search(Domain))
        ]
      end
    }.

test_add(Domain) ->
    {ok, created} = uce_event_solr_search:add(#uce_event{id=utils:random(),
                                                         domain=Domain,
                                                         datetime=utils:now(),
                                                         location={"testmeeting", Domain},
                                                         from={"chuck_norris", Domain},
                                                         type="test_solr_event",
                                                         metadata=[{"text","This is a test event."}]}).

test_search(Domain) ->
    timer:sleep(2000),
    ok = case uce_event_solr_search:list(Domain, {"", Domain}, ["This"], {"", Domain}, [], 0, infinity, "", 0, 1, asc) of
             {error, Reason} ->
                 {error, Reason};
             {ok, [_]} ->
                 ok
         end.
