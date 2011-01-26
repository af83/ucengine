-record(uce_event, {
	  %% Id
	  id = none,
	  % date (ms from epoch)
	  datetime = none,
	  %% location = [Meeting]
	  location = [""],
	  %% From: uid|brick
	  from,
	  %% To : string
	  to = "all",
	  %% Type event : string
	  type,
	  %% parent id
	  parent = "",
	  %% MetaData : list
	  metadata = []}).

-record(uce_trigger, {
	  %% Event type
	  type,
	  %% [meeting]
	  location = [""],
	  %% action : {Action, Params}
	  action}).

-record(uce_presence, {
	  sid = [],
	  % user id
	  uid,
	  %% authification method
	  auth,
	  % timeout
	  last_activity = undefined,
	  % resource
	  resource,
          %% list meetings joined by user
	  meetings = [],
	  %% MetaData : list
	  metadata = []}).

-record(uce_meeting, {
	  %% uce meeting id {meeting_name})
	  id,
	  %% start_date and end_date format : {{Year, Month, Day}, {Hours, Minutes, Seconds}}
	  %% or timestamp
	  start_date = none,
	  end_date = none,
	  roster = [],
	  %% [{"description",Desc}, {"language",Lang}, ... ]
	  metadata = []
	 }).

-record(uce_file, {
	  % file id
	  id,
	  % name
	  name,
	  % [meeting]
	  location = [""],
	  % path
	  uri = [],
	  % mime type
	  mime = "text/plain",
	  % name as send by the browser
	  metadata = []
	 }).

-record(uce_user, {
	  uid,
	  auth,
	  credential,
	  metadata = []}).

-record(uce_acl, {
	  uid,
	  action,
	  object,
	  location=[""],
	  conditions=[]}).

-record(uce_route, {
	  module = [],
	  title = [],
	  desc = [],
	  path = [],
	  types = [],
	  method,
	  regexp,
	  callbacks}).

-define(TIMEOUT, 5000).

-define(VERSION, config:get(version)).

-define(HOST, config:get(host)).
-define(PORT, config:get(port)).
-define(BASE_URL, "http://" ++ ?HOST ++ ":" ++ integer_to_list(?PORT) ++ "/api/" ++ config:get(version)).

-define(SESSION_TIMEOUT, (config:get(presence_timeout) * 1000)).

-define(DEBUG(Format, Args),
		error_logger:info_msg("DEBUG: ~p:~p: " ++ Format, [?MODULE, ?LINE] ++ Args)).

-define(INFO_MSG(Format, Args),
		error_logger:info_msg("~p:~p: " ++ Format, [?MODULE, ?LINE] ++ Args)).

-define(WARNING_MSG(Format, Args),
		error_logger:warning_msg("~p:~p: " ++ Format, [?MODULE, ?LINE] ++ Args)).

-define(ERROR_MSG(Format, Args),
		error_logger:error_msg("~p:~p: " ++ Format, [?MODULE, ?LINE] ++ Args)).

-define(CRITICAL_MSG(Format, Args),
		error_logger:critical_msg("~p:~p: " ++ Format, [?MODULE, ?LINE] ++ Args)).

-define(UCE_SCHEMA_LOCATION, "uce_schema_v1.xsd").
-define(UCE_XMLNS, "http://ucengine.org").

-define(DEFAULT_TIME_INTERVAL, 600000).

-define(NEVER_ENDING_MEETING, 0).

-define(PRESENCE_EXPIRED_EVENT, "internal.presence.expired").
