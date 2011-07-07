-type domain() :: list().
-type sid() :: list().
-type uid() :: list().
-type meeting() :: list().

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
          metadata = []}).

-record(uce_presence, {
          %% presenceid
          id = none,
          %% user id
          user,
          %% authification method
          auth,
          %% session timeout
          timeout = 0,
          %% last ping
          last_activity = 0,
          %% resource
          resource,
          %% list meetings joined by user
          meetings = [],
          %% MetaData : list
          metadata = []}).

-record(uce_meeting, {
          %% uce meeting id
          id = none,
          %% start_date and end_date format : ms since epoch
          start_date = none,
          end_date = none,
          roster = [],
          %% [{"description",Desc}, {"language",Lang}, ... ]
          metadata = []}).

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
          metadata = []
         }).

-record(uce_user, {
          %% User uid
          id = none,
          %% name
          name,
          auth,
          credential = "",
          metadata = [],
          roles=[]}).

-record(uce_role, {
          id = "",
          acl=[]}).

-record(uce_access, {
          action,
          object,
          conditions=[]}).

-record(uce_infos, {
          domain = none,
          metadata = []}).

-record(uce_route, {
          method,
          path,
          callback}).

-record(file_upload, {
          fd,
          filename,
          uri}).

-define(TIMEOUT, 5000).

-define(VERSION, "0.5").

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

-define(UCE_SCHEMA_LOCATION, "uce_schema_v1.xsd").
-define(UCE_XMLNS, "http://ucengine.org").

-define(DEFAULT_TIME_INTERVAL, 600000).

-define(NEVER_ENDING_MEETING, 0).

-define(PRESENCE_EXPIRED_EVENT, "internal.presence.expired").

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
