-record(uce_event, {
          %% eventid
          id = none,
          %% date (ms from epoch)
          datetime = undefined,
          %% location = [Meeting]
          location = "",
          %% From: uid|brick
          from,
          %% To : string
          to = "",
          %% Type event : string
          type,
          %% parent id
          parent = "",
          %% MetaData : list
          metadata = {struct, []}}).

-record(uce_presence, {
          %% presenceid
          id = none,
          %% user id
          user,
          %% authentication method
          auth,
          %% session timeout
          timeout = 0,
          %% last ping
          last_activity = 0,
          %% list meetings joined by user
          meetings = [],
          %% nb streams open
          streams = 0}).

-record(uce_meeting, {
          %% uce meeting id
          id = none,
          roster = [],
          %% [{"description",Desc}, {"language",Lang}, ... ]
          metadata = {struct, []}}).

-record(uce_file, {
          % fileid
          id = none,
          % name
          name,
          % Meeting
          location = "",
          % path
          uri = [],
          %% date (ms from epoch)
          datetime = undefined,
          % mime type
          mime = "text/plain",
          % name as send by the browser
          metadata = {struct, []}
         }).

-record(uce_user, {
          %% User uid
          id = none,
          %% name
          name,
          auth,
          credential = "",
          metadata = {struct, []},
          roles=[]}).

-record(uce_role, {
          id = "",
          acl=[]}).

-record(uce_access, {
          action,
          object,
          conditions=[]}).

-record(uce_route, {
          method,
          path,
          content_type = any,
          callback}).

-record(file_upload, {
          fd,
          filename,
          uri}).

% Types
-type domain()         :: string().
-type meeting_id()     :: string().
-type meeting()        :: #uce_meeting{}.
-type event_id()       :: string().
-type event()          :: #uce_event{}.
-type user()           :: #uce_user{}.
-type presence()       :: #uce_presence{}.
-type sid()            :: string().
-type uid()            :: string().
-type role_id()        :: string().
-type role()           :: #uce_role{}.
-type access()         :: #uce_access{}.
-type route()          :: #uce_route{}.
-type timestamp()      :: integer().


-define(VERSION, "0.6").

-define(SESSION_TIMEOUT, (config:get(presence_timeout) * 1000)).

-define(DEBUG(Format, Args),
        uce_log:debug(Format, [?MODULE, ?LINE], Args)).

-define(INFO_MSG(Format, Args),
        uce_log:info(Format, [?MODULE, ?LINE], Args)).

-define(WARNING_MSG(Format, Args),
        uce_log:warning(Format, [?MODULE, ?LINE], Args)).

-define(ERROR_MSG(Format, Args),
        uce_log:error(Format, [?MODULE, ?LINE], Args)).

-define(CRITICAL_MSG(Format, Args),
        uce_log:critical(Format, [?MODULE, ?LINE], Args)).

-define(COUNTER(Name), (
    fun() ->
        case config:get(metrics) of
            ok ->
                metrics_counter:incr(Name);
            _ -> ok
        end
    end())).

-define(TIMER_APPEND(Name, Timer), (
    fun() ->
        case config:get(metrics) of
            ok ->
                metrics_gauge:append_timer(Name, Timer);
            _ -> ok
        end
    end())).

-define(GAUGE_APPEND(Gauge, Value), (
    fun() ->
        case config:get(metrics) of
            ok ->
                metrics_gauge:append(Gauge, Value);
            _ -> ok
        end
    end()
)).


% Backends

-define(PUBSUB_MODULE, mnesia_pubsub).
-define(AUTH_MODULE(Module),
        (fun() ->
                 list_to_atom(Module ++ "_auth")
         end())).
-define(DB_MODULE,
        (fun() ->
                 list_to_atom(atom_to_list(?MODULE) ++ "_"
                              ++ atom_to_list(config:get(db)))
         end())).
-define(SEARCH_MODULE,
        (fun() ->
                 list_to_atom(atom_to_list(?MODULE) ++ "_"
                              ++ atom_to_list(config:get(search)) ++ "_search")
         end())).

-define(REMOVE_ID_FROM_RECORD(Params, Record),
        case Params of
            Params when is_list(Params) ->
                lists:map(fun(#Record{id={Id, _Domain}} = Param) ->
                                  Param#Record{id=Id}
                          end, Params);
            Params when is_record(Params, Record) ->
                {Id, _Domain} = Params#Record.id,
                Params#Record{id=Id}
        end).
