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
-module(uce_event_solr_search).

-author('thierry.bomandouki@af83.com').

-export([add/1, commit/0, list/7, delete/2]).

-include("uce.hrl").

-define(DEFAULT_HOST, "http://localhost:8983/solr").
-define(SOLR_UPDATE, "/update").
-define(SOLR_SELECT, "/select?").

-define(META_PREFIX, "metadata_").

add(Event) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    ibrowse:send_req(Host ++ ?SOLR_UPDATE, [], post, to_solrxml(Event)),
    {ok, created}.

commit() ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    Commit = lists:flatten(xmerl:export_simple_element({commit, []}, xmerl_xml)),
    ibrowse:send_req(Host ++ ?SOLR_UPDATE, [], post, Commit),
    {ok, commited}.

%% Encode event in solrxml format which be used to add solr index
to_solrxml(#uce_event{id=Id,
                      domain=Domain,
                      datetime=Datetime,
                      location={Location, _},
                      from={From, _},
                      to={To, _},
                      type=Type,
                      metadata=Metadata}) ->

    LocationElement = [{field, [{name,"location"}], [Location]}],

    MetadataFlattenElement =
        [{field, [{name, "metadata"}], [lists:flatten([Value ++ " " || {_, Value} <- Metadata])]}],

    MetadataElement =
        lists:map(fun({Key, Value}) ->
                          {field, [{name, "metadata_" ++ Key}], [Value]}
                  end,
                  Metadata),

    DocElements = [{field, [{name,"id"}], [Id]},
                   {field, [{name,"domain"}], [Domain]},
                   {field, [{name,"datetime"}], [integer_to_list(Datetime)]},
                   {field, [{name,"type"}], [Type]},
                   {field, [{name,"to"}], [To]},
                   {field, [{name,"from"}], [From]}] ++
        LocationElement ++
        MetadataFlattenElement ++
        MetadataElement,

    Add = {add, [], [{doc, [], DocElements}]},
    lists:flatten(xmerl:export_simple_element(Add, xmerl_xml)).

params_to_query([{Key, Value}|Tail]) ->
    case Key of
        [] ->
            [];
        _ ->
            Key ++ ":"
    end ++ Value ++ case Tail of
                        [] ->
                            [];
                        _ ->
                            " +" ++ params_to_query(Tail)
                    end.

list({Location, Domain}, Search, {From, _}, Type, Start, End, Parent) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),

    DomainSelector = [{"domain", Domain}],

    LocationSelector =
        if
            Location /= "" ->
                [{"location", Location}];
            true ->
                []
        end,

    FromSelector =
        if
            From /= "" ->
                [{"from", From}];
            true ->
                []
        end,

    ParentSelector = 
        if
            Parent /= '_' ->
                [{"parent", Parent}];
            true ->
                []
        end,

    SearchSelector =
        if
            Search /= '_' ->
                [{"metadata", string:join([Key || Key <- Search], "+")}];
            true ->
                []
        end,

    TypeSelector =
        if
            Type /= '_' ->
                [{"type", Type}];
            true ->
                []
        end,

    TimeRange =
        if
            Start /= 0, End /= infinity ->
                "[" ++ integer_to_list(Start) ++ " TO " ++ integer_to_list(End) ++ "]";
            Start /= 0 ->
                "[" ++ integer_to_list(Start) ++ " TO *]";
            End /= infinity ->
                "[* TO " ++ integer_to_list(End) ++ "]";
            true ->
                []
        end,

    TimeSelector =
        if
            Start /= 0; End /= infinity ->
                 [{"facet", "on"}, {"facet.field", "datetime"}, {"fq", "datetime:" ++ TimeRange}];
            true ->
                []
        end,

    Query = [{"q", params_to_query(LocationSelector ++
                                       DomainSelector ++
                                       ParentSelector ++
                                       FromSelector ++
                                       TypeSelector ++
                                       SearchSelector)}],

    EncodedParams = [yaws_api:url_encode(Elem) ++ "=" ++ yaws_api:url_encode(Value) ||
                        {Elem, Value} <- Query ++ TimeSelector ++ [{"wt", "json"}]],

    case ibrowse:send_req(Host ++ ?SOLR_SELECT ++ string:join(EncodedParams, "&"), [], get) of
        {ok, _, _, JSON} ->
            {ok, json_to_events(mochijson:decode(JSON))};
        {error, _} ->
            {error, bad_parameters}
    end.

json_to_events({struct, JSON}) ->
    {"response", {struct, ResponseJSON}} = lists:keyfind("response", 1, JSON),
    {"docs", {array, DocsJSON}} = lists:keyfind("docs", 1, ResponseJSON),
    make_list_json_events(DocsJSON).

make_list_json_events([]) ->
    [];
make_list_json_events([{struct, Elems}|Tail]) ->
    case utils:get(Elems,
                   ["id", "domain", "datetime", "location","from", "to", "type", "parent"],
                   [none,
                    none,
                    none,
                    {array, [""]},
                    none,
                    {array, ["all"]},
                    none,
                    {array, [""]}]) of

        [none, _, _, _, _, _, _, _, _] ->
            {error, bad_record};
        [_, none, _, _, _, _, _, _, _] ->
            {error, bad_record};
        [_, _, none, _, _, _, _, _, _] ->
            {error, bad_record};
        [_, _, _, _, _, none, _, _, _] ->
            {error, bad_record};
        [_, _, _, _, _, _, _, none, _] ->
            {error, bad_record};

        [{array, [Id]},
         {array, [Domain]},
         {array, [Datetime]},
         {array, [Location]},
         {array, [From]},
         {array, [To]},
         {array, [Type]},
         {array, [Parent]}] ->
            FlatMetadata =
                lists:filter(fun({Name, _}) ->
                                     if
                                         length(Name) < length(?META_PREFIX) ->
                                             false;
                                         true ->
                                             SubName = string:substr(Name, 1, length(?META_PREFIX)),
                                             if
                                                 SubName == ?META_PREFIX ->
                                                     true;
                                                 true ->
                                                     false
                                             end
                                     end
                             end,
                             Elems),
            Metadata = lists:map(fun({Name, {array, [Value]}}) ->
                                         {string:substr(Name, length(?META_PREFIX) + 1, length(Name)),
                                          Value}
                                 end,
                                 FlatMetadata),
            [#uce_event{id=Id,
                        domain=Domain,
                        datetime=list_to_integer(Datetime),
                        location={Location, Domain},
                        from={From, Domain},
                        to={To, Domain},
                        type=Type,
                        parent=Parent,
                        metadata=Metadata}] ++ make_list_json_events(Tail)
    end.

delete(_Domain, Id) ->
    [Host] = utils:get(config:get(solr), [host], [?DEFAULT_HOST]),
    ibrowse:send_req(Host ++ ?SOLR_UPDATE, [], post, "<delete><query>"++ Id ++"</query></delete>"),
    {ok, deleted}.

