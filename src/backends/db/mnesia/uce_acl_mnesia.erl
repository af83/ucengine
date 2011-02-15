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
-module(uce_acl_mnesia).

-author('victor.goya@af83.com').

-behaviour(gen_uce_acl).

-export([init/0, drop/0]).

-export([add/1,
         delete/5,
         list/3]).

-include("uce.hrl").

init() ->
    catch mnesia:create_table(uce_acl,
                              [{disc_copies, [node()]},
                               {type, bag},
			       {attributes, record_info(fields, uce_acl)}]).

add(#uce_acl{}=ACL) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(ACL)
                            end) of
        {atomic, _} ->
            {ok, created};
        {aborted, Reason} ->
            {error, Reason}
    end.

delete(User, Object, Action, Location, Conditions) ->
    case mnesia:transaction(fun() ->
                                    mnesia:delete_object(#uce_acl{user=User,
                                                                  object=Object,
                                                                  action=Action,
                                                                  location=Location,
                                                                  conditions=Conditions})
                            end) of
        {atomic, _} ->
            {ok, deleted};
        {aborted, _} ->
            throw({error, bad_parameters})
    end.
	
list(User, Object, Action) ->
    case mnesia:transaction(fun() ->
				    mnesia:match_object(#uce_acl{user=User,
                                                 object=Object,
                                                 action=Action,
                                                 location='_',
                                                 conditions='_'})
                            end) of
        {aborted, _} ->
            throw({error, bad_parameters});
        {atomic, ACL} ->
            {ok, AllActions} =
                case Action of
                    "all" ->
                        {ok, []};
                    _ ->
                        ?MODULE:list(User, "all", Object)
                end,
            {ok, AllObjects} =
                case Object of
                    "all" ->
                        {ok, []};
                    _ ->
                        ?MODULE:list(User, Action, "all")
                end,
            {ok, ACL ++ AllActions ++ AllObjects}
    end.

drop() ->
    mnesia:clear_table(uce_acl).
