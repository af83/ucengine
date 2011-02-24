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
-module(search_controller).

-export([init/0, search/4]).

-include("uce.hrl").
-include_lib("yaws/include/yaws_api.hrl").

init() ->
    [#uce_route{method='GET',
                regexp="/search/([^/]+)",
                callbacks=[{?MODULE, search,
                            ["uid",
                             "sid",
                             "searchTerms",
                             "startIndex",
                             "startPage",
                             "count"],
                            [required, required, "", 0, 1, infinity],
                            [string, string, string, integer, integer, [integer, atom]]}]}].

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

search(Domain, [_Type], [Uid, _Sid, SearchTerms, StartIndex, StartPage, Count], Arg) ->
%    {ok, true} = uce_presence:assert({Uid, Domain}, Sid),

    [{"type", Type},
     {"start", Start},
     {"end", End},
     {"location", Location},
     {"from", From},
     {"to", _To},
     {"parent", Parent},
     {"keywords", Keywords}] =
        extract_terms(SearchTerms,
                      ["type", "start", "end", "location", "from", "to", "parent"],
                      ["", 0, infinity, "", "", "", ""]),
    
    {ok, Events} = uce_event:list({Location, Domain},
                                  Keywords,
                                  {From, Domain},
                                  string:tokens(Type, ","),
                                  {Uid, Domain},
                                  Start,
                                  End,
                                  Parent),

    ItemsPerPage =
        case Count of
            infinity ->
                length(Events);
            _ ->
                Count
        end,

    EventPage = helpers:paginate(event_helpers:sort(Events),
                                  ItemsPerPage,
                                  StartPage,
                                  asc),

    {abs_path, Path} = Arg#arg.req#http_request.path,
    Link = lists:flatten("http://" ++ Arg#arg.headers#headers.host ++ Path),
    atom_helpers:atom(Link,
                      "",
                      SearchTerms,
                      StartIndex,
                      StartPage,
                      ItemsPerPage,
                      EventPage).
