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
-module(uce_yaws).

-include("uce.hrl").
-include_lib("yaws/include/yaws.hrl").

-export([child_spec/0]).

-import(lists, [member/2]).

child_spec() ->
    [{DefaultHost, _Config}|_Hosts] = config:get(hosts),
    {ok, _SCList, _GC, ChildSpecs} = embedded_start_conf(config:get(DefaultHost, wwwroot),
                        [{servername, DefaultHost},
                         {listen, config:get(bind_ip)},
                         {port, config:get(port)},
                         {access_log, true},
                         {partial_post_size, nolimit},
                         {opaque, DefaultHost},
                         {appmods, [{"/api/" ++ ?VERSION, uce_appmod}]}],
                        [{flags, [{auth_log, false},
                                  {copy_errlog, false},
                                  {pick_first_virthost_on_nomatch, false},
                                  {debug, false}
                                 ]},
                         {logdir, config:get(log_dir)},
                         {cache_refresh_secs, config:get(cache_refresh)}]),
    ChildSpecs.

%
% Backported from yaws 1.89
% We support currently 1.88 and more
% Remove this if we drop the support of yaws 1.88
%
embedded_start_conf(DocRoot, SL, GL)
  when is_list(DocRoot), is_list(SL), is_list(GL) ->
    embedded_start_conf(DocRoot, SL, GL, "default").
embedded_start_conf(DocRoot, SL, GL, Id)
  when is_list(DocRoot), is_list(SL), is_list(GL) ->
    case application:load(yaws) of
        ok -> ok;
        {error, {already_loaded,yaws}} -> ok;
        _ -> exit("cannot load yaws")
    end,
    ok = application:set_env(yaws, embedded, true),
    ok = application:set_env(yaws, id, Id),
    ChildSpecs = child_specs(),
    GC = yaws:create_gconf(GL, Id),
    SCList = case SL of
                  [] ->
                      [[]];
                  [Cnf|_] when is_tuple(Cnf) ->
                      [[yaws:create_sconf(DocRoot, SL)]];
                  [Cnf|_] when is_list(Cnf) ->
                      [[yaws:create_sconf(DocRoot, SLItem)] || SLItem <- SL]
              end,
    SoapChild = yaws_config:add_yaws_soap_srv(GC, false),

    %% In case a server is started before any configuration has been set,
    %% this makes it possible to get hold of the 'pending' configuration.
    %% (see for example the start of the yaws_session_server)
    ok = application:set_env(yaws, embedded_conf, [{sclist,SCList},{gc,GC}]),
    {ok, SCList, GC, ChildSpecs ++ SoapChild}.

child_specs() ->
    YawsLog = {yaws_log, {yaws_log, start_link, []},
               permanent, 5000, worker, [yaws_log]},

    YawsServArgs = [_Env = get_app_args()],
    YawsServ = {yaws_server, {yaws_server, start_link, YawsServArgs},
                permanent, 120000, worker, [yaws_server]},

    %% and this guy will restart auxiliary procs that can fail
    Sup = {yaws_sup_restarts,
           {yaws_sup_restarts, start_link, []},
           transient, infinity, supervisor, [yaws_sup_restarts]},

    [YawsLog, YawsServ, Sup].

get_app_args() ->
    AS=init:get_arguments(),
    Debug = case application:get_env(yaws, debug) of
                undefined ->
                    member({yaws, ["debug"]}, AS);
                {ok, Val} ->
                    Val
            end,
    Trace = case application:get_env(yaws, trace) of
                undefined ->
                    case {member({yaws, ["trace", "http"]}, AS),
                          member({yaws, ["trace", "traffic"]}, AS)} of
                        {true, _} ->
                            {true, http};
                        {_, true} ->
                            {true, traffic};
                        _ ->
                            false
                    end;
                {ok, http} ->
                    {true, http};
                {ok, traffic} ->
                    {true, traffic};
                _ ->
                    false
            end,
    TraceOutput = case application:get_env(yaws, traceoutput) of
                      undefined ->
                          member({yaws, ["traceoutput"]}, AS);
                      {ok, Val3} ->
                          Val3
                  end,
    Conf = case application:get_env(yaws, conf) of
               undefined ->
                   find_c(AS);
               {ok, File} ->
                   {file, File}
           end,
    RunMod = case application:get_env(yaws, runmod) of
                 undefined ->
                     find_runmod(AS);
                 {ok,Mod} ->
                     {ok,Mod}
             end,
    Embedded = case application:get_env(yaws, embedded) of
                   undefined ->
                       false;
                   {ok, Emb} ->
                       Emb
               end,
    Id = case application:get_env(yaws, id) of
             undefined ->
                 "default";
             {ok, Id0} when is_atom(Id0) ->
                 atom_to_list(Id0);
             {ok, Id0} ->
                 Id0
         end,

    #env{debug = Debug, trace = Trace,
         traceoutput = TraceOutput, conf = Conf,
         runmod = RunMod, embedded = Embedded, id = Id}.


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
find_c([{conf, [File]} |_]) ->
    {file, File};
find_c([_|T]) ->
    find_c(T);
find_c([]) ->
    false.


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
find_runmod([{runmod, [Mod]} |_]) ->
    {ok,l2a(Mod)};
find_runmod([_|T]) ->
    find_runmod(T);
find_runmod([]) ->
    false.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
l2a(L) when is_list(L) -> list_to_atom(L);
l2a(A) when is_atom(A) -> A.
