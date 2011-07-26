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
-module(json_helpers).

-compile({no_auto_import,[error/2]}).

-include("uce.hrl").

-export([format_response/4,
         unexpected_error/1,
         error/2,
         error/3,
         ok/1,
         true/1,
         false/1,
         created/1,
         created/2,
         json/2,
         json/3,
         to_json/2,
         to_struct/1]).

format_response(Status, Headers, Content) ->
    format_response(Status, "application/json", Headers, to_struct(Content)).

format_response(Status, ContentType, Headers, Content) ->
    Body = mochijson:encode(Content),
    [{status, Status},
     {content, ContentType, lists:flatten(Body)}] ++ Headers.

add_cors_headers(Domain) ->
    cors_helpers:format_cors_headers(Domain).

unexpected_error(Domain) ->
    format_response(500, add_cors_headers(Domain), [{error, unexpected_error}]).

error(Domain, Reason) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Code, add_cors_headers(Domain), [{error, Reason}]).
error(Domain, Reason, Infos) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Code, add_cors_headers(Domain), [{error, Reason}, {infos, Infos}]).

ok(Domain) ->
    format_response(200, add_cors_headers(Domain), [{result, ok}]).

true(Domain) ->
    format_response(200, add_cors_headers(Domain), [{result, "true"}]).

false(Domain) ->
    format_response(200, add_cors_headers(Domain), [{result, "false"}]).

created(Domain) ->
    format_response(201, add_cors_headers(Domain), [{result, created}]).

created(Domain, Id) ->
    format_response(201, add_cors_headers(Domain), [{result, Id}]).

json(Domain, Content) ->
    json(Domain, 200, Content).

json(Domain, Status, Content) ->
    format_response(Status, add_cors_headers(Domain), [{result, to_json(Domain, Content)}]).
%%
%% Transform usual records to JSON
%%
to_json(Domain, Records) when is_list(Records) ->
    {array, [to_json(Domain, Record) || Record <- Records]};
to_json(_Domain, #uce_access{action=Action,
                             object=Object,
                             conditions=Conditions}) ->
    {struct,
     [{action, Action},
      {object, Object},
      {conditions, Conditions}]};
to_json(Domain, #uce_meeting{id=Name,
                             start_date=StartDate,
                             end_date=EndDate,
                             metadata=Metadata}) ->
    {struct, [{name, Name},
              {domain, Domain},
              {start_date, StartDate},
              {end_date, case EndDate of
                             ?NEVER_ENDING_MEETING ->
                                 "never";
                             _ ->
                                 integer_to_list(EndDate)
                         end},
              {metadata, Metadata}]};
to_json(Domain, #uce_event{id=Id,
                           datetime=Datetime,
                           location=Location,
                           from=From,
                           type=Type,
                           to=To,
                           parent=Parent,
                           metadata=Metadata}) ->
    JSONTo = case To of
                 "" ->
                     [];
                 ToId ->
                     [{to, ToId}]
             end,

    JSONLocation = case Location of
                       "" ->
                           [];
                       Meeting ->
                           [{location, Meeting}]
                   end,
    JSONParent = case Parent of
                     "" ->
                         [];
                     _ ->
                         [{parent, Parent}]
                 end,
    Metadata2 = case Metadata of
                    {struct, _} ->
                        Metadata;
                    _ ->
                        {struct, Metadata}
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
         [{metadata, Metadata2}]};
to_json(Domain, #uce_file{id=Id,
                          name=Name,
                          location=Location,
                          uri=Uri,
                          metadata=Metadata}) ->
    JSONLocation = [{location, Location}],
    {struct, [{id, Id},
              {domain, Domain},
              {name, Name},
              {uri, Uri}] ++ JSONLocation ++
         [{metadata, Metadata}]};
to_json(Domain, #uce_presence{id=Id,
                              user=User,
                              auth=Auth,
                              metadata=Metadata}) ->
    {struct,
     [{id, Id},
      {domain, Domain},
      {user, User},
      {auth, Auth},
      {metadata, Metadata}]};
to_json(Domain, #uce_user{id=Id,
                          name=Name,
                          auth=Auth,
                          metadata=Metadata}) ->
    {struct, [{uid, Id},
              {name, Name},
              {domain, Domain},
              {auth, Auth},
              {metadata, to_struct(Metadata)}]};
to_json(_Domain, Json) ->
    Json.

-spec to_struct([{string() | atom(), any()}]) -> {struct, [{string() | atom(), any()}]}.
to_struct({struct, _} = Struct) ->
    Struct;
to_struct(Proplist) ->
    {struct, Proplist}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unexpected_error_test() ->
    ?assertMatch([{status, 500}, {content, "application/json", "{\"error\":\"unexpected_error\"}"},
                 {header, "Access-Control-Allow-Origin: *"},
                 {header, "Access-Control-Allow-Methods: GET, POST, PUT, DELETE"},
                 {header, "Access-Control-Allow-Headers: X-Requested-With"}], unexpected_error("")).

error_test() ->
    ?assertMatch([{status, 400}, {content, "application/json", "{\"error\":\"bad_parameters\"}"},
                  {header, "Access-Control-Allow-Origin: *"},
                  {header, "Access-Control-Allow-Methods: GET, POST, PUT, DELETE"},
                  {header, "Access-Control-Allow-Headers: X-Requested-With"}], error("", bad_parameters)),
    ?assertMatch([{status, 500}, {content, "application/json", "{\"error\":\"hello_world\"}"},
                  {header, "Access-Control-Allow-Origin: *"},
                  {header, "Access-Control-Allow-Methods: GET, POST, PUT, DELETE"},
                  {header, "Access-Control-Allow-Headers: X-Requested-With"}], error("", "hello_world")).

format_response_test() ->
    ?assertMatch([{status, 200}, {content, "application/json", "{}"}], format_response(200, [], [])),
    ?assertMatch([{status, 200}, {content, "application/json", "{}"}, {header, "X-Plop: plop"}, {header, "Host: ucengine.org"}], format_response(200, [{header, "X-Plop: plop"}, {header, "Host: ucengine.org"}], [])).

-endif.
