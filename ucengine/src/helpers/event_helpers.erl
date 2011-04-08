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
-module(event_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([sort/1, sort/2, to_json/1, from_json/1]).

sort(Events) ->
    ?MODULE:sort(Events, asc).
sort(Events, asc) ->
    lists:sort(fun(Event1, Event2) ->
                       Event1#uce_event.datetime < Event2#uce_event.datetime
               end,
               Events);
sort(Events, desc) ->
    lists:sort(fun(Event1, Event2) ->
                       Event1#uce_event.datetime > Event2#uce_event.datetime
               end,
               Events).

to_json(#uce_event{id={Id, Domain},
                   datetime=Datetime,
                   location=Location,
                   from={From, _},
                   type=Type,
                   to=To,
                   parent=Parent,
                   metadata=Metadata}) ->
    JSONTo = case To of
                 {"", _} ->
                     [];
                 {ToId, _} ->
                     [{to, ToId}]
             end,

    JSONLocation = case Location of
                       {"", _} ->
                           [];
                       {Meeting, Domain} ->
                           [{location, Meeting}]
                   end,
    JSONParent = case Parent of
                     "" ->
                         [];
                     _ ->
                         [{parent, Parent}]
                 end,
    {struct,
     [{type, Type},
      {domain, Domain},
      {datetime, Datetime},
      {id, Id}] ++
         JSONLocation ++
         JSONTo ++
         [{from, From}] ++
         JSONParent ++
         [{metadata, {struct, Metadata}}]};

to_json(Events)
  when is_list(Events) ->
    {array, [?MODULE:to_json(Event) || Event <- Events]}.

from_json({struct, Event}) ->
    case utils:get(Event, ["id", "datetime", "from", "meeting", "type", "parent", "metadata"]) of
        {error, Reason} ->
            {error, Reason};
        [Id, Datetime, From, Meeting, Type, Parent, {struct, Metadata}] ->
            {_, Domain} = Meeting,
            #uce_event{id={Id, Domain},
                       datetime=Datetime,
                       from=From,
                       location=Meeting,
                       type=Type,
                       parent=Parent,
                       metadata=Metadata}
    end.
