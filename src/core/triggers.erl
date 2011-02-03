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
-module(triggers).

-include("uce.hrl").

-author('victor.goya@af83.com').

-export([init/0, terminate/0, add/1, list/2, run/2, run/3]).

init() ->
    mnesia:create_table(uce_trigger,
                        [{ram_copies, [node()]},
                         {type, bag},
                         {attributes, record_info(fields, uce_trigger)}]).

terminate() ->
    ok.

add(#uce_trigger{} = Trigger) ->
    mnesia:dirty_write(Trigger).

list(Location, Type) ->
    lists:filter(fun(#uce_trigger{location=TriggerLocation}) ->
                         case TriggerLocation of
                             '_' ->
                                 true;
                             Location ->
                                 true;
                             _ ->
                                 false
                         end
                 end,
                 lists:filter(fun(#uce_trigger{type=TriggerType}) ->
                                      case TriggerType of
                                          '_' ->
                                              true;
                                          Type ->
                                              true;
                                          _ ->
                                              false
                                      end
                              end,
                              ets:tab2list(uce_trigger))).

run([], _) ->
    [];
run([Trigger|Tl], Event) ->
    {Action, Params} = Trigger#uce_trigger.action,
    Result = case Action of
                 {url, Url} ->
                     case catch httpc:request(post,
                                              {Url,
                                               [],
                                               [],
                                               "{\"event\":" ++
                                                   lists:flatten(event:to_json(Event)) ++
                                                   "}"
                                              },
                                              [{timeout, ?TIMEOUT}],
                                              []) of
                         {'EXIT', _} ->
                             {error, bad_parameters};
                         _ ->
                             ok
                     end;
                 {Module, Method} ->
                     case catch Module:Method(Event, Params) of
                         ok ->
                             ok;
                         {_, Reason} ->
                             {error, Reason};
                         _ ->
                             {error, unexpected_error}
                     end
             end,
    [{Action, Result}] ++ ?MODULE:run(Tl, Event).

run(Location, Type, Event) ->
    ?MODULE:run(triggers:list(Location, Type), Event).
