-record(uce_event, {
          %% Id
          id = none,
          % Domain (vhost)
          domain,
          %% date (ms from epoch)
          datetime = undefined,
          %% location = [Meeting]
          location = {"", ""},
          %% From: uid|brick
          from,
          %% To : string
          to = {"", ""},
          %% Type event : string
          type,
          %% parent id
          parent = "",
          %% MetaData : list
          metadata = []}).

-record(uce_presence, {
          %% Id
          id = none,
          %% domain
          domain,
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
          id = {"", ""},
          %% start_date and end_date format : ms since epoch
          start_date = none,
          end_date = none,
          roster = [],
          %% [{"description",Desc}, {"language",Lang}, ... ]
          metadata = []}).

-record(uce_file, {
          % file id
          id = none,
          % domain
          domain,
          % name
          name,
          % {Meeting, Domain}
          location = {"", ""},
          % path
          uri = [],
          % mime type
          mime = "text/plain",
          % name as send by the browser
          metadata = []
         }).

-record(uce_user, {
          %% User (name, domain)
          id = none,
          auth,
          credential = "",
          metadata = []}).

-record(uce_infos, {
          domain = none,
          metadata = []}).

-record(uce_acl, {
          user,
          action,
          object,
          location={"", ""},
          conditions=[]}).

-record(uce_route, {
          method,
          regexp,
          callback}).

-define(TIMEOUT, 5000).

-define(VERSION, "0.4").

-define(SESSION_TIMEOUT, (config:get(presence_timeout) * 1000)).

-define(DEBUG(Format, Args),
        uce_log:debug(Format, [?MODULE, ?LINE], Args)).

-define(INFO_MSG(Format, Args),
        error_logger:info_msg(Format, [?MODULE, ?LINE] ++ Args)).

-define(WARNING_MSG(Format, Args),
        error_logger:warning_msg(Format, [?MODULE, ?LINE] ++ Args)).

-define(ERROR_MSG(Format, Args),
        error_logger:error_msg(Format, [?MODULE, ?LINE] ++ Args)).

-define(CRITICAL_MSG(Format, Args),
        error_logger:critical_msg(Format, [?MODULE, ?LINE] ++ Args)).

-define(UCE_SCHEMA_LOCATION, "uce_schema_v1.xsd").
-define(UCE_XMLNS, "http://ucengine.org").

-define(DEFAULT_TIME_INTERVAL, 600000).

-define(NEVER_ENDING_MEETING, 0).

-define(PRESENCE_EXPIRED_EVENT, "internal.presence.expired").

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
