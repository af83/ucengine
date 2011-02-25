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
-module(paginate).

-author('victor.goya@af83.com').

-export([index/3, sort/2, paginate/3]).

index(Count, Index, Page) ->
    case Count of
        infinity ->
            Index;
        _ ->
            (Count * (Page - 1)) + Index
    end.

sort(List, Order) ->
    case Order of
        asc ->
            List;
        desc ->
            lists:reverse(List)
    end.

paginate(List, Start, Count) ->
    Max = if
              Count == infinity ->
                  length(List);
              true ->
                  Count
          end,
    if
        Start > length(List) ->
            [];
        true ->
            lists:sublist(List, Start + 1, Max)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

paginate_test() ->
    List = lists:seq(0, 12),
    ?assertEqual(lists:seq(0, 9), paginate(List, 0, 10)),
    ?assertEqual(lists:seq(0, 4), paginate(List, 0, 5)),
    ?assertEqual(lists:seq(2, 11), paginate(List, 2, 10)),
    ?assertEqual(lists:seq(5, 9), paginate(List, 5, 5)).

-endif.
