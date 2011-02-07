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

-export([init/0, get/3, update/3]).

init() ->
    [#uce_route{module="Infos",
                title="Get infos",
                desc="Get informations",
                path="/infos",
                method='GET',
                regexp="/infos",
                types=[],
                callbacks=[{?MODULE, get, [], [], [], []}]},

     #uce_route{module="Infos",
                title="Update infos",
                desc="Update informations",
                path="/infos",
                method='POST',
                regexp="/infos",
                types=[],
                callbacks=[{presence_controller, check,
                            ["uid", "sid"],
                            [required, required],
                            [string, string],
                            [user, presence]},
                           {?MODULE, update,
                            ["uid", "metadata"],
                            [required, []],
                            [string, dictionary],
                            [user, any]}]
               }].

%%
%% Get domain informations
%% Return a json object containing the domain's metadata. Can be empty.
%%
get(_UrlParams, _Params, Arg) ->
    {ok, Result} = uce_infos:get(utils:domain(Arg)),
    json_helpers:json({struct, Result}).

%%
%% Update domain informations
%% Return ok in case of success.
%%
update(_UrlParams, [Uid, Metadata], Arg) ->
    case uce_acl:check(utils:domain(Arg), Uid, "infos", "update") of
        {ok, true} ->
            uce_infos:update(utils:domain(Arg), Metadata),
            json_helpers:ok();
        {ok, false} ->
            {error, unauthorized}
    end.
