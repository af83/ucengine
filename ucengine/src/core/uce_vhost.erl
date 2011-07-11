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
-module(uce_vhost).

-behaviour(gen_server).

-include("uce.hrl").

-export([start_link/1, add_user/2]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%%
%% Public api
%%

start_link(Domain) ->
    gen_server:start_link({local, uce_vhost_sup:name(Domain, "vhost")}, ?MODULE, [Domain], []).

add_user(Domain, Uid) ->
    gen_server:call(uce_vhost_sup:name(Domain, "vhost"), {add_user, Uid}).

%%
%% gen_server callbacks
%%

init([Domain]) ->
    setup_db(Domain),
    setup_roles(Domain),
    setup_root_role(Domain),
    setup_bricks(Domain),
    setup_admin(Domain),
    {ok, Domain}.

handle_call({add_user, Uid}, _From, Domain) ->
    {ok, User} = uce_user:get(Domain, Uid),
    case uce_vhost_user_sup:start_child(Domain, [Domain, User]) of
        {ok, Pid} ->
            {reply, {ok, Pid}, Domain};
        {error, Reason} ->
            case gproc:lookup_local_name({Domain, uid, Uid}) of
                undefined ->
                    {error, Reason};
                Pid ->
                    {reply, {ok, Pid}, Domain}
            end
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {reply, State}.

code_change(_,State,_) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Private functions
%%

setup_db(Domain) ->
    DBBackend = config:get(Domain, db),
    DBConfig = config:get(Domain, DBBackend),
    DBBackendModule = list_to_atom(lists:concat([DBBackend, "_db"])),
    DBBackendModule:init(Domain, DBConfig).

setup_roles(Domain) ->
    case catch uce_role:add(Domain, #uce_role{id="default", acl=[]}) of
        {ok, created} ->
            ok;
        {error, conflict} ->
            ok;
        {error, Reason} ->
            throw({error, Reason})
    end,
    setup_role(Domain, config:get(Domain, roles)).

setup_role(_, undefined) ->
    ok;
setup_role(_, []) ->
    ok;
setup_role(Domain, [{Name, ConfigACL}|Tail]) ->
    ACL = lists:map(fun({Action, Object, Conditions}) ->
                            #uce_access{action=Action,
                                        object=Object,
                                        conditions=Conditions}
                    end,
                    ConfigACL),
    case catch uce_role:add(Domain, #uce_role{id=Name, acl=ACL}) of
        {ok, created} ->
            setup_role(Domain, Tail);
        {error, conflict} ->
            uce_role:update(Domain, #uce_role{id=Name, acl=ACL}),
            setup_role(Domain, Tail);
        {error, Reason} ->
            throw({error, Reason})
    end.

setup_root_role(Domain) ->
    case catch uce_role:add(Domain, #uce_role{id="root",
                                              acl=[#uce_access{action="all", object="all"}]}) of
        {ok, created} ->
            ok;
        {error, conflict} ->
            ok;
        {error, _} = Error ->throw(Error)
    end.

setup_root_user(Domain, #uce_user{} = User) ->
    case catch uce_user:add(Domain, User) of
        {ok, UId} ->
            uce_user:add_role(Domain, UId, {"root", []});
        {error, conflict} ->
            ok;
        {error, _} = Error -> throw(Error)
    end.

setup_bricks(Domain) ->
    lists:foreach(fun({Name, Token}) ->
                          setup_root_user(Domain, #uce_user{name=Name,
                                                            auth="token",
                                                            credential=Token,
                                                            metadata=[]})
                  end,
                  config:get(Domain, bricks)).


setup_admin(Domain) ->
    Admin = config:get(Domain, admin),
    Name = proplists:get_value(uid, Admin),
    Auth = proplists:get_value(auth, Admin),
    Credential = proplists:get_value(credential, Admin),
    Metadata = proplists:get_value(metadata, Admin, []),
    setup_root_user(Domain, #uce_user{name=Name,
                                      auth=Auth,
                                      credential=Credential,
                                      metadata=Metadata}).
