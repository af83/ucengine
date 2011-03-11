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

-export([name/1, start_link/1]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

name(Domain) ->
    list_to_atom(lists:concat([?MODULE, "_", Domain])).

start_link(Domain) ->
    gen_server:start_link({local, name(Domain)}, ?MODULE, [Domain], []).

%%
%% gen_server callbacks
%%

init([Domain]) ->
    setup_db(Domain),
    setup_bricks(Domain),
    setup_admin(Domain),
    {ok, Domain}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

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
    case DBBackend of
        undefined -> throw({error, no_database});
        mnesia -> catch mnesia_db:init([]);
        _ ->
            case DBConfig of
                undefined -> nothing;
                {_, PoolConfig} ->
                    DBBackendModule = list_to_atom(atom_to_list(DBBackend) ++ "_db"),
                    DBBackendModule:init({Domain, PoolConfig}) %% we use Hostname as the pool name.
            end
    end.

setup_bricks(Domain) ->
    lists:foreach(fun({Name, Token}) ->
                          catch uce_user:add(#uce_user{id={Name, Domain},
                                                       auth="token",
                                                       credential=Token,
                                                       metadata=[]}),
                          catch uce_acl:add(#uce_acl{user={Name, Domain},
                                                     action="all",
                                                     object="all",
                                                     conditions=[]})
                  end,
                  config:get(Domain, bricks)).


setup_admin(Domain) ->
    Admin = config:get(Domain, admin),
    Uid = proplists:get_value(uid, Admin),
    Auth = proplists:get_value(auth, Admin),
    Credential = proplists:get_value(credential, Admin),
    Metadata = proplists:get_value(metadata, Admin, []),
    catch uce_user:add(#uce_user{id={Uid, Domain},
                                 auth=Auth,
                                 credential=Credential,
                                 metadata=Metadata}),
    catch uce_acl:add(#uce_acl{user={Uid, Domain},
                               action="all",
                               object="all",
                               conditions=[]}).


%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

name_test() ->
    ?assertEqual(uce_vhost_localhost, name(localhost)),
    ?assertEqual('uce_vhost_example.com', name('example.com')).

-endif.
