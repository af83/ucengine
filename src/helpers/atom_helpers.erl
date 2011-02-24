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
-module(atom_helpers).

-export([atom/7]).

-include("uce.hrl").

random_hexa(0) ->
    [];
random_hexa(Length) ->
    Number = crypto:rand_uniform(0,16),
    Hexa = lists:nth(Number + 1, [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]),
    [Hexa] ++ random_hexa(Length - 1).

uuid() ->
    random_hexa(8) ++ "-" ++
        random_hexa(4) ++ "-" ++
        random_hexa(4) ++ "-" ++
        random_hexa(4) ++ "-" ++
        random_hexa(12).

atom(Link, Updated, SearchTerms, StartIndex, StartPage, ItemsPerPage, Events) ->
    Entries =
        lists:map(fun(#uce_event{id=Id,
                                 domain=Domain,
                                 datetime=Datetime,
                                 from={From, _},
                                 type=Type,
                                 location={Location, _},
                                 parent=Parent,
                                 metadata=Metadata}) ->
                          {'entry', [], [{title, [], [Id]},
                                         {id, [], ["urn:uuid:" ++ uuid()]},
                                                % TODO: use http://tools.ietf.org/html/rfc3339
                                         {updated, [], [integer_to_list(Datetime)]},
                                         {domain, [], [Domain]},
                                         {from, [], [From]},
                                         {type, [], [Type]},
                                         {location, [], [Location]},
                                         {parent, [], [Parent]},
                                         {content, [], [{list_to_atom(Key), [], [Value]} || {Key, Value} <- Metadata]}]}
                  end,
                  Events),
    Feed = {feed, [{xmlns, "http://www.w3.org/2005/Atom"},
                   {'xmlns:opensearch', "http://a9.com/-/spec/opensearch/1.1/"}],
            [{title, [], ["U.C.Engine search"]},
             {link, [], [Link]},
             {updated, [], [Updated]},
             {id, [], ["urn:uuid:" ++ uuid()]},
             {'opensearch:totalResults', [], [integer_to_list(length(Entries))]},
             {'opensearch:startIndex', [], [integer_to_list(StartIndex)]},
             {'opensearch:itemsPerPage', [], [integer_to_list(ItemsPerPage)]},
             {'opensearch:Query', [{role, "request"},
                                   {searchTerms, SearchTerms},
                                   {startPage, integer_to_list(StartPage)}], []}] ++
                Entries},
        [{status, 200},
         {content,
          "application/atom+xml",
          lists:flatten(xmerl:export_simple_element(Feed, xmerl_xml))}].
