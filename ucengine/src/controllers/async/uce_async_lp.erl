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
-module(uce_async_lp).

-author('victor.goya@af83.com').

-export([wait/9]).

-include("uce.hrl").

wait(Domain, Location, Search, From, Types, Uid, Start, End, Parent) ->
    Self = self(),
    spawn(fun() ->
                  {ok, JSONEvents} = uce_async:listen(Domain,
                                                      Location,
                                                      Search,
                                                      From,
                                                      Types,
                                                      Uid,
                                                      Start,
                                                      End,
                                                      Parent),
                  yaws_api:stream_chunk_deliver(Self, list_to_binary(JSONEvents)),
                  yaws_api:stream_chunk_end(Self)
          end),
    Timeout = (config:get(long_polling_timeout) + 1) * 1000,
    {streamcontent_with_timeout, "application/json", <<>>, Timeout}.
