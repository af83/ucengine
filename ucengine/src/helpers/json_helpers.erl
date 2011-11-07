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
         json/3,
         json/4,
         to_json/2,
         to_struct/1]).

format_response(Response, Status, Content) ->
    format_response(Response, Status, "application/json", to_struct(Content)).

format_response(Response, Status, ContentType, Content) ->
    Body = lists:flatten(mochijson:encode(Content)),
    Response#uce_response{status=Status, content={content, ContentType, Body}}.

unexpected_error(Response) ->
    format_response(Response, 500, [{error, unexpected_error}]).

error(Response, Reason) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Response, Code, [{error, Reason}]).
error(Response, Reason, Infos) ->
    Code = http_helpers:error_to_code(Reason),
    format_response(Response, Code, [{error, Reason}, {infos, Infos}]).

ok(Response) ->
    format_response(Response, 200, [{result, ok}]).

true(Response) ->
    format_response(Response, 200, [{result, "true"}]).

false(Response) ->
    format_response(Response, 200, [{result, "false"}]).

created(Response) ->
    format_response(Response, 201, [{result, created}]).

created(Response, Id) ->
    format_response(Response, 201, [{result, Id}]).

json(Response, Domain, Content) ->
    json(Response, Domain, 200, Content).

json(Response, Domain, Status, Content) ->
    format_response(Response, Status, [{result, to_json(Domain, Content)}]).
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
                             metadata=Metadata}) ->
    {struct, [{name, Name},
              {domain, Domain},
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
                              auth=Auth}) ->
    {struct,
     [{id, Id},
      {domain, Domain},
      {user, User},
      {auth, Auth}]};
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
    ?assertMatch(#uce_response{status=500, content={content, "application/json", "{\"error\":\"unexpected_error\"}"}}, unexpected_error(#uce_response{})).

error_test() ->
    ?assertMatch(#uce_response{status=400, content={content, "application/json", "{\"error\":\"bad_parameters\"}"}}, error(#uce_response{}, bad_parameters)),
    ?assertMatch(#uce_response{status=500, content={content, "application/json", "{\"error\":\"hello_world\"}"}}, error(#uce_response{}, "hello_world")).

format_response_test() ->
    ?assertMatch(#uce_response{status=200, content={content, "application/json", "{}"}}, format_response(#uce_response{}, 200, [])).

-endif.
