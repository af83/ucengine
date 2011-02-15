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
-module(uce_async).

-author('victor.goya@af83.com').

-export([listen/9]).

-include("uce.hrl").
-include("uce_async.hrl").

listen(Location, Search, From, Types, Uid, Start, End, Parent, Socket) ->
    ?PUBSUB_MODULE:subscribe(self(), Location, Search, From, Types, Uid, Start, End, Parent),
    Res = receive
              {message, _} ->
                  case uce_event:list(Location,
                                      Search,
                                      From,
                                      Types,
                                      Uid,
                                      Start,
                                      End,
                                      Parent) of
                      {error, Reason} ->
                          throw({error, Reason});
                      {ok, Events} ->
                          JSONEvent = mochijson:encode({struct,
                                                        [{result,
                                                          event_helpers:to_json(Events)}]}),
                          yaws_api:stream_process_deliver_final_chunk(Socket,
                                                                      list_to_binary(JSONEvent)),
                          ok
                  end;
              _ ->
                  ok
          after
              config:get(long_polling_timeout) * 1000 ->
                  JSONEmpty = mochijson:encode({struct, [{result, {array, []}}]}),
                  yaws_api:stream_process_deliver_final_chunk(Socket,
                                                              list_to_binary(JSONEmpty)),
                  ok
          end,
    ?PUBSUB_MODULE:unsubscribe(self()),
    Res.
