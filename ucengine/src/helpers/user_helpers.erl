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
-module(user_helpers).

-author('victor.goya@af83.com').

-include("uce.hrl").

-export([pretty_print/2]).

pretty_print(Users, Format)
  when is_list(Users) ->
    lists:flatten([pretty_print(User, Format) ++ "--~n" || User <- Users]);
pretty_print(#uce_user{id=Uid,
                       name=Name,
                       auth=Auth,
                       metadata=Metadata,
                       roles=Roles}, flat) ->
    Out = [io_lib:format("Id: ~s~n", [Uid]),
           io_lib:format("Name: ~s~n", [Name]),
           io_lib:format("Authentification method: ~s~n", [Auth])],
    StrMetadata =
        if
            Metadata == [] ->
                ["Metadata: none~n"];
            true ->
                [io_lib:format("Metadata:~n", [])] ++
                    [ io_lib:format("\t~s: ~s~n", [Key, Value]) || {Key, Value} <- Metadata ]
        end,
    StrRoles =
        if
            Roles == [] ->
                ["Roles: none~n"];
            true ->
                [io_lib:format("Roles:~n", [])] ++
                    [ if
                          Location == "" ->
                              io_lib:format("\t~s in all locations~n", [Role]);
                          true ->
                              io_lib:format("\t~s in ~s~n", [Role, Location])
                      end || {Role, Location} <- Roles ]
        end,
    lists:flatten(Out ++ StrMetadata ++ StrRoles).
