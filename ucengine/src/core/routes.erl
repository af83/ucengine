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
-module(routes).

-include("uce.hrl").

-export([init/0,
         get/1,
         get/2]).

init() ->
    Routes = setup_routes(),
    TableId = ets:new(uce_routes, [bag, public, named_table]),
    [ets:insert(TableId, Route) || Route <- Routes],
    ok.

%%
%% Search route matching a path
%%
get(Path) ->
    get('_', Path).
%%
%% Search route matching a path and the method
%%
get(Method, Path) ->
    route(Method, Path, ets:tab2list(uce_routes)).

route(_, _, []) ->
    {error, not_found};
route('_', Path, [#uce_route{callback=Callback} = Route|Routes]) ->
    case match_path(Path, Route) of
        false ->
            route('_', Path, Routes);
        {ok, Binds, List} ->
            {ok, lists:reverse(Binds) ++ List, Callback}
    end;
route(Method, Path, [#uce_route{method=Method, callback=Callback} = Route|Routes]) ->
    case match_path(Path, Route) of
        false ->
            route(Method, Path, Routes);
        {ok, Binds, List} ->
            {ok, lists:reverse(Binds) ++ List, Callback}
    end;
route(Method, Path, [#uce_route{}|Routes]) ->
    route(Method, Path, Routes).

match_path("/"++ Path, #uce_route{path=PathRule}) ->
    List = re:split(Path, "[/]", [{return,list}, trim]),
    case list_match(List, PathRule, []) of
        false ->
            false;
        {true, Binds, undefined} ->
            {ok, Binds, []};
        {true, Binds, List2} ->
            {ok, Binds, List2}
    end.

% We are too lazy
% we have stolen this code from cowboy
% https://github.com/extend/cowboy/blob/master/src/cowboy_dispatcher.erl
% Cowboy: Loïc Hoguin, Hans Ulrich Niedermann, Anthony Ramine

-type bindings() :: list({atom(), binary()}).
-type path_tokens() :: list(binary()).
-type match_rule() :: '_' | '*' | list(binary() | '_' | atom()).

-spec list_match(path_tokens(), match_rule(), bindings())
         -> {true, bindings(), undefined | path_tokens()} | false.
%% Atom '...' matches any trailing path, stop right now.
list_match(List, ['...'], Binds) ->
    {true, Binds, List};
%% Atom '_' matches anything, continue.
list_match([_E|Tail], ['_'|TailMatch], Binds) ->
    list_match(Tail, TailMatch, Binds);
%% Both values match, continue.
list_match([E|Tail], [E|TailMatch], Binds) ->
    list_match(Tail, TailMatch, Binds);
%% Bind E to the variable name V and continue.
list_match([E|Tail], [V|TailMatch], Binds) when is_atom(V) ->
    list_match(Tail, TailMatch, [{V, E}|Binds]);
%% Match complete.
list_match([], [], Binds) ->
    {true, Binds, undefined};
%% Values don't match, stop.
list_match(_List, _Match, _Binds) ->
    false.

setup_routes([]) ->
    [];
setup_routes([Controller|Controllers]) ->
    Controller:init() ++ setup_routes(Controllers).

% TODO : make list of controllers more generic
setup_routes() ->
    setup_routes([user_controller,
                  presence_controller,
                  meeting_controller,
                  role_controller,
                  event_controller,
                  file_controller,
                  time_controller,
                  infos_controller,
                  search_controller]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

route_without_method_test() ->
    Routes = [#uce_route{method='GET',
                         path=["user"],
                         callback={?MODULE, get, []}},
              #uce_route{method='POST',
                         path=["user", name],
                         callback={?MODULE, update_user, []}},
              #uce_route{method='PUT',
                         path=["user", name, '...'],
                         callback={?MODULE, put_user_plop, []}}],
    ?assertMatch({error, not_found}, route('_', "/user/", [])),
    ?assertMatch({ok, [], _}, route('_', "/user/", Routes)).

route__with_method_test() ->
    Routes = [#uce_route{method='GET',
                         path=["user"],
                         callback={?MODULE, get, []}},
             #uce_route{method='GET',
                         path=["user", name],
                         callback={?MODULE, get_user, []}},
             #uce_route{method='GET',
                         path=["user", name, id],
                         callback={?MODULE, get_user_name_id, []}},
              #uce_route{method='POST',
                         path=["user", name],
                         callback={?MODULE, update_user, []}},
             #uce_route{method='PUT',
                         path=["user", name, '...'],
                         callback={?MODULE, put_user_plop, []}}],
    ?assertMatch({error, not_found}, route('GET', "/user/", [])),
    ?assertMatch({ok, [], {?MODULE, get, []}}, route('GET', "/user/", Routes)),
    ?assertMatch({ok, [], {?MODULE, get, []}}, route('GET', "/user", Routes)),
    ?assertMatch({ok, [{name, "plop"}], {?MODULE, get_user, []}}, route('GET', "/user/plop", Routes)),
    ?assertMatch({ok, [{name, "plop"}, {id, "myid"}], {?MODULE, get_user_name_id, []}}, route('GET', "/user/plop/myid", Routes)),
    ?assertMatch({ok, [{name, "plop"}], {?MODULE, update_user, []}}, route('POST', "/user/plop", Routes)),
    ?assertMatch({ok, [{name, "plop"}, "plip"], {?MODULE, put_user_plop, []}}, route('PUT', "/user/plop/plip", Routes)).

-endif.
