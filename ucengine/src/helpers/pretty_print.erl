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
-module(pretty_print).

-include("uce.hrl").

-export([print/2]).

%%
%% Print uce records
%%
print(Records, flat) when is_list(Records) ->
    lists:flatten([print(Record, flat) ++ "--~n" || Record <- Records]);
print(#uce_meeting{id=Id,
                   start_date=Start,
                   end_date=End,
                   metadata=Metadata}, flat) ->
    Out = [io_lib:format("Id: ~s~n", [Id]),
           io_lib:format("Start: ~p~n", [Start]),
           io_lib:format("End: ~p~n", [End])],
    StrMetadata = print_metadata(Metadata),
    lists:flatten(Out ++ StrMetadata);
print(#uce_user{id=Uid,
                name=Name,
                auth=Auth,
                metadata=Metadata,
                roles=Roles}, flat) ->
    Out = [io_lib:format("Id: ~s~n", [Uid]),
           io_lib:format("Name: ~s~n", [Name]),
           io_lib:format("Authentification method: ~s~n", [Auth])],
    StrMetadata = print_metadata(Metadata),
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

print_metadata({struct, Metadata}) ->
    print_metadata(Metadata);
print_metadata([]) ->
    ["Metadata: none~n"];
print_metadata(Metadata) ->
    [io_lib:format("Metadata:~n", [])] ++
        [ io_lib:format("\t~s: ~s~n", [Key, Value]) || {Key, Value} <- Metadata ].
