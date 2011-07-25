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
-module(uce_async_stream).

-export([wait/7]).

-include("uce.hrl").

wait(Domain, Location, Search, From, Types, Parent, PreviousEvents) ->
    YawsPid = self(),
    spawn(fun() ->
                  send_events(YawsPid, Domain, PreviousEvents),
                  ?PUBSUB_MODULE:subscribe(self(), Domain, Location, From, Types, Parent),
                  listen(YawsPid,
                         Domain,
                         Location,
                         Search,
                         From,
                         Types,
                         Parent)
          end),
    {streamcontent_with_timeout, "text/event-stream", <<>>, infinity}.

listen(YawsPid, Domain, Location, Search, From, Types, Parent) ->
    {ok, Event} = uce_async:listen(Domain,
                                   Location,
                                   Search,
                                   From,
                                   Types,
                                   Parent,
                                   infinity),
    send_events(YawsPid, Domain, [Event]),
    listen(YawsPid, Domain, Location, Search, From, Types, Parent).

send_events(_, _, []) ->
    ok;
send_events(YawsPid, Domain, [Event|Events]) ->
    yaws_api:stream_chunk_deliver(YawsPid, "data: "),
    yaws_api:stream_chunk_deliver(YawsPid, mochijson:encode(json_helpers:to_json(Domain, Event))),
    yaws_api:stream_chunk_deliver(YawsPid, "\n\n"),
    send_events(YawsPid, Domain, Events).
