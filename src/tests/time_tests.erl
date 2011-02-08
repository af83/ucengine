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
-module(time_tests).

-include_lib("eunit/include/eunit.hrl").

time_test_() ->
    { setup,
      fun fixtures:setup/0,
      fun fixtures:teardown/1,
      fun([_, BaseUrl, _]) ->
	      [?_test(test_get(BaseUrl))]
      end}.

test_get(BaseUrl) ->
    {struct, [{"result", Time}]} = tests_utils:get(BaseUrl, "/time", []),
    Diff = Time - utils:now(),
    if
	Diff > 1000 ->
	    throw({error, too_much_delay, Diff});
	true ->
	    nothing
    end.
