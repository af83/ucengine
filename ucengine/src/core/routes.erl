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
-module(routes).

-include("uce.hrl").

-export([init/0,
         get/1,
         get/2,
         get/3]).

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

get(Method, Path, undefined) -> % default value of the #headers record (in yaw_api.hrl)
    route(Method, Path, undefined, ets:tab2list(uce_routes));
get(Method, Path, "") ->
    route(Method, Path, "", ets:tab2list(uce_routes));
get(Method, Path, ContentType) when is_list(ContentType) ->
    [ContentType2|_] = string:tokens(ContentType, ";"),
    route(Method, Path, ContentType2, ets:tab2list(uce_routes)).

route(Method, Path, Routes) ->
    route(Method, Path, "", Routes).

-spec route(Method :: atom(), Path :: string(), ContentType :: string(), Routes :: [route()])
    -> {error, not_found} | {ok, route(), list(atom()|string())}.
route(_, _, _, []) ->
    {error, not_found};
route('_', Path, ContentType, [#uce_route{callback=Callback} = Route|Routes]) ->
    case match_path(Path, Route) of
        false ->
            route('_', Path, ContentType, Routes);
        {ok, Binds, List} ->
            {ok, lists:reverse(Binds) ++ List, Callback}
    end;
route(Method, Path, ContentType, [#uce_route{
        method=Method,
        content_type=any} = Route|Routes]) ->
    case match_path(Path, Route) of
        false ->
            route(Method, Path, ContentType, Routes);
        {ok, Binds, List} ->
            {ok, Route, lists:reverse(Binds) ++ List}
    end;
route(Method, Path, ContentType, [#uce_route{
        method=Method,
        content_type=ContentType} = Route|Routes]) ->
    case match_path(Path, Route) of
        false ->
            route(Method, Path, ContentType, Routes);
        {ok, Binds, List} ->
            {ok, Route, lists:reverse(Binds) ++ List}
    end;
route(Method, Path, ContentType, [#uce_route{}|Routes]) ->
    route(Method, Path, ContentType, Routes).

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
    R = lists:nth(1, Routes),
    ?assertMatch({ok, R, []}, route('GET', "/user/", Routes)),
    ?assertMatch({ok, R, []}, route('GET', "/user", Routes)),
    R2 = lists:nth(2, Routes),
    ?assertMatch({ok, R2, [{name, "plop"}]}, route('GET', "/user/plop", Routes)),
    R3 = lists:nth(3, Routes),
    ?assertMatch({ok, R3, [{name, "plop"}, {id, "myid"}]}, route('GET', "/user/plop/myid", Routes)),
    R4 = lists:nth(4, Routes),
    ?assertMatch({ok, R4, [{name, "plop"}]}, route('POST', "/user/plop", Routes)),
    R5 = lists:nth(5, Routes),
    ?assertMatch({ok, R5, [{name, "plop"}, "plip"]}, route('PUT', "/user/plop/plip", Routes)).

route_with_content_type_test() ->
     Routes = [#uce_route{method='POST',
                          content_type="application/json",
                          path=["user", name],
                          callback={?MODULE, update_user, []}},
               #uce_route{method='POST',
                          path=["user", name],
                          callback={?MODULE, update_user_inline, []}}],
    R1 = lists:nth(1, Routes),
    ?assertMatch({ok, R1, [{name, "42"}]}, route('POST', "/user/42", "application/json", Routes)),
    R2 = lists:nth(2, Routes),
    ?assertMatch({ok, R2, _}, route('POST', "/user/42", "", Routes)),
    ?assertMatch({ok, R2, _}, route('POST', "/user/42", "text/plain", Routes)).

-endif.
