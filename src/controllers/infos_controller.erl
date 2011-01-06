-module(infos_controller).

-include("uce.hrl").

-export([init/0, get/3, update/3]).

init() ->
    [#uce_route{module="Infos",
                title="Get infos",
                desc="Get informations",
                path="/infos",
                method='GET',
                regexp="/infos",
                types=[],
                callbacks=[{?MODULE, get, [], [], [], []}]},

     #uce_route{module="Infos",
                title="Update infos",
                desc="Update informations",
                path="/infos",
                method='POST',
                regexp="/infos",
                types=[],
                callbacks=[{presence_controller, check,
                            ["uid", "sid"],
                            [required, required],
                            [string, string],
                            [user, presence]},
                           {?MODULE, update,
                            ["uid", "metadata"],
                            [required, []],
                            [string, dictionary],
                            [user, any]}]
               }].

%%
%% Get domain informations
%% Return a json object containing the domain's metadata. Can be empty.
%%
get(_UrlParams, _Params, _Arg) ->
    {ok, Result} = uce_infos:get(),
    json_helpers:json({struct, Result}).

%%
%% Update domain informations
%% Return ok in case of success.
%%
update(_UrlParams, [Uid, Metadata], _Arg) ->
    case uce_acl:check(Uid, "infos", "update") of
        {ok, true} ->
            uce_infos:update(Metadata),
            json_helpers:ok();
        {ok, false} ->
            {error, unauthorized}
    end.
