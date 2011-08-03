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
-module(search_controller).

-export([init/0, search/4]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

init() ->
    [#uce_route{method='GET',
                path=["search", '_'],
                callback={?MODULE, search,
                          [{"uid", required, string},
                           {"sid", required, string},
                           {"searchTerms", "", string},
                           {"startIndex", 0, integer},
                           {"startPage", 1, integer},
                           {"count", 10, integer},
                           {"order", "asc", string}]}}].

extract_terms(SearchTerms, [Term|Terms], [Default|Defaults]) ->
    {ok, Regexp} = re:compile("(^|.* )" ++ Term ++ ":([^ ]+)(.*)"),
    case re:run(SearchTerms, Regexp, [{capture, all, list}]) of
        {match, [_, Start, Value, End]} ->
            [{Term, Value}] ++ extract_terms(Start ++ End, Terms, Defaults);
        nomatch ->
            [{Term, Default}] ++ extract_terms(SearchTerms, Terms, Defaults);
        Error ->
            ?ERROR_MSG("search: ~p", [Error]),
            throw({error, bad_parameters})
    end;
extract_terms(SearchTerms, [], []) ->
    [{"keywords", string:tokens(SearchTerms, " ")}].

search(Domain, [], [Uid, Sid, SearchTerms, StartIndex, StartPage, Count, Order], Arg) ->
    {ok, true} = uce_presence:assert(Domain, Uid, Sid),

    [{"type", Type},
     {"start", DateStart},
     {"end", DateEnd},
     {"location", Location},
     {"from", From},
     {"to", _To},
     {"parent", Parent},
     {"keywords", Keywords}] =
        extract_terms(SearchTerms,
                      ["type", "start", "end", "location", "from", "to", "parent"],
                      ["", "0", infinity, "", "", "", ""]),

    {ok, true} = uce_access:assert(Domain, Uid, Location, "event", "list",
                                   [{"from", From}]),

    DateEndInt = case DateEnd of
                     infinity ->
                         infinity;
                     A when is_list(A) ->
                         list_to_integer(A)
                 end,

    Start = uce_paginate:index(Count, StartIndex, StartPage),
    {ok, NumTotal, Events} = uce_event:search(Domain,
                                              Location,
                                              Keywords,
                                              From,
                                              string:tokens(Type, ","),
                                              Uid,
                                              list_to_integer(DateStart),
                                              DateEndInt,
                                              Parent,
                                              Start,
                                              Count,
                                              list_to_atom(Order)),

    {abs_path, Path} = Arg#arg.req#http_request.path,
    Link = lists:concat(["http://", Arg#arg.headers#headers.host, Path]),

    Entries = json_helpers:to_json(Domain, Events),
    Feed = {struct, [{'link', Link},
                     {'totalResults', NumTotal},
                     {'startIndex', StartIndex},
                     {'itemsPerPage', Count},
                     {'Query', {struct, [{role, "request"},
                                         {searchTerms, SearchTerms},
                                         {startPage, StartPage}]}},
                     {'entries', Entries}]},
    json_helpers:json(Domain, Feed).
