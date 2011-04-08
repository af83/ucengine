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
-module(infos_controller).

-include("uce.hrl").

-export([init/0, get/4, update/4]).

init() ->
    [#uce_route{method='GET',
                regexp="/infos",
                callback={?MODULE, get,
                          [{"uid", required, string},
                           {"sid", required, string}]}},

     #uce_route{method='PUT',
                regexp="/infos",
                callback={?MODULE, update,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"metadata", [], dictionary}]}}].

%%
%% Get domain informations
%% Return a json object containing the domain's metadata. Can be empty.
%%
get(Domain, _UrlParams, [Uid, Sid], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "infos", "get"),
    {ok, #uce_infos{domain=Domain, metadata=Metadata}} = uce_infos:get(Domain),
    json_helpers:json(Domain, {struct, [{domain, Domain},
                                        {metadata, {struct, Metadata}}]}).

%%
%% Update domain informations
%% Return ok in case of success.
%%
update(Domain, _UrlParams, [Uid, Sid, Metadata], _) ->
    {ok, true} = uce_presence:assert(Domain, {Uid, Domain}, {Sid, Domain}),
    {ok, true} = uce_access:assert(Domain, {Uid, Domain}, {"", ""}, "infos", "update"),
    {ok, updated} = uce_infos:update(Domain, #uce_infos{domain=Domain, metadata=Metadata}),
    json_helpers:ok(Domain).
