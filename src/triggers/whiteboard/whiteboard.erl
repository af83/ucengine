-module(whiteboard).

-author('tbomandouki@af83.com').

-export([snapshot/1,
	 auto_save/0]).

-define(WHITEBOARD_WIDTH, 400).
-define(WHITEBOARD_HEIGHT, 400).

-define(CIRCLE, 3).
-define(RECTANGLE, 4).

-define(WHITEBOARD_EVENT, "whiteboard_draw_event").
-define(WHITEBOARD_SNAPSHOT_EVENT, "whiteboard_snapshot_event").
-define(WHITEBOARD_CLEAR_EVENT, "whiteboard_clear_event").

-define(ENCRE_WHITEBOARD_AUTOSAVE_SNAPSHOT_INTERVAL, config:get(uce_garbage_session_interval)).

-include("uce.hrl").

snapshot({Org, Meeting}) ->
	Time = utils:now(),
	LastWhiteboard = event:last(Org, Meeting, ?WHITEBOARD_EVENT),
	LastSnapshot = event:last(Org, Meeting, ?WHITEBOARD_SNAPSHOT_EVENT),
	Continue = case LastSnapshot of
				   none -> true;
				   [] -> true;
				   _ -> LastSnapshotTime = LastSnapshot#uce_event.datetime,
						LastWhiteboardTime = LastWhiteboard#uce_event.datetime,
						if
							LastSnapshotTime >= LastWhiteboardTime -> false;
							true -> true
						end
			   end,
	case Continue of
		true ->
			LastWhiteboardClear = event:last(Org, Meeting, ?WHITEBOARD_CLEAR_EVENT),
			Start = case LastWhiteboardClear of
						#uce_event{datetime=Datetime} -> Datetime;
						_ -> '_'
					end,
			Events = event:get(Org, Meeting, '_', ?WHITEBOARD_EVENT, Start, Time),
			case Events of 
				[] -> nothing;
				_ ->
					ImageToSave = egd:render(make_snapshot(egd:create(?WHITEBOARD_WIDTH, ?WHITEBOARD_HEIGHT), Events)),
					Now = utils:now(),
					%%EventId = utils:eventid(whiteboard_snapshot_event, Time),
					FileName = "whiteboard_snapshot_" ++ integer_to_list(Now) ++ ".png",
					Path = config:get(datas) ++ "/" ++ Org ++ "/" ++ Meeting ++ "/",
					egd:save(ImageToSave, Path ++ FileName),
					event:add(#uce_event{location=[Org, Meeting],
							       from="whiteboard", 
							       type=list_to_atom(?WHITEBOARD_SNAPSHOT_EVENT), 
							       metadata=[{"directory", Path}, {"filename", FileName}]}),
					mnesia:dirty_write({uce_file, {{Org, Meeting}, FileName}, Path ++ FileName, [{"description","whiteboard snapshot"}, {"timecode",Now}]})
			end;
		_ -> nothing
	end.

auto_save() ->
	case meeting:list('_', "opened") of
		[] -> nothing;
		Meetings ->
			[ snapshot(Meeting#uce_meeting.id) || Meeting <- Meetings]
	end,
	timer:apply_after(?ENCRE_WHITEBOARD_AUTOSAVE_SNAPSHOT_INTERVAL, ?MODULE, auto_save, []).

%% Internal functions

make_snapshot(Image, [HdEvent | Tl]) ->
	make_snapshot(event2egd(Image, HdEvent), Tl);
make_snapshot(Image, []) -> Image.


% {"event":"clearCanvas","args":{}}

%
event2egd(Image, #uce_event{type=?WHITEBOARD_EVENT, metadata=Metadata}) ->
	{value, {"wevent",SerializedEvent}} = lists:keysearch("wevent", 1, Metadata),
	{struct, [
			  {"event", TypeWhiteboardEvent},
			  {"args", {struct, ParamsWhiteboardEvent}}
			 ]
	} = mochijson:decode(SerializedEvent),
	if
		TypeWhiteboardEvent /= "clearCanvas" ->
			{value, {"startPos", {struct, [{"x", StartPosX}, {"y", StartPosY}]}}} = lists:keysearch("startPos", 1, ParamsWhiteboardEvent),
			{value, {"curPos", {struct, [{"x", CurPosX}, {"y", CurPosY}]}}} = lists:keysearch("curPos", 1, ParamsWhiteboardEvent),
			{value, {"drawColor", Color}} = lists:keysearch("drawColor", 1, ParamsWhiteboardEvent),
			{value, {"lineWidth", Width}} = lists:keysearch("lineWidth", 1, ParamsWhiteboardEvent),
			Rgba = make_rgba(re:split(re:replace(re:replace(Color, "rgba?\\(", "", [{return,list}]), "\\)", "", [{return,list}]), ",", [{return, list}])),
			RgbColor = egd:color(Rgba),
			case TypeWhiteboardEvent of
				"drawPencil" ->
					egd:line(Image, {StartPosX, StartPosY}, {CurPosX, CurPosY}, RgbColor),
					lists:foreach(fun(P)-> egd:line(Image, {StartPosX, StartPosY+P}, {CurPosX, CurPosY+P}, RgbColor) end, lists:seq(1, Width));
				"drawBrush" ->
					egd:line(Image, {StartPosX, StartPosY}, {CurPosX, CurPosY}, RgbColor),
					lists:foreach(fun(P)-> egd:line(Image, {StartPosX, StartPosY+P}, {CurPosX, CurPosY+P}, RgbColor) end, lists:seq(1, Width));
				"drawAction" ->
					{value, {"actionNumber", ActionNumber}} = lists:keysearch("actionNumber", 1, ParamsWhiteboardEvent),
					case ActionNumber of 
						?RECTANGLE -> egd:filledRectangle(Image, {StartPosX, StartPosY}, {CurPosX, CurPosY}, RgbColor);
						?CIRCLE -> egd:filledEllipse(Image, {StartPosX, StartPosY}, {CurPosX, CurPosY}, RgbColor);
						_ -> egd:line(Image, {StartPosX, StartPosY}, {CurPosX, CurPosY}, RgbColor)
					end;
				_ -> nothing
			end;
		true -> nothing
	end,
	Image.

make_rgba([R, G, B]) ->
	Red = case catch list_to_integer(R) of
			  {'EXIT', _} -> list_to_float(R);
			  RedValue -> RedValue
		  end,
	Green = case catch list_to_integer(G) of
				{'EXIT', _} -> list_to_float(G);
				GreenValue -> GreenValue
			end,
	Blue = case catch list_to_integer(B) of
			   {'EXIT', _} -> list_to_float(B);
			   BlueValue -> BlueValue
		   end,
	{Red, Green, Blue};
make_rgba([R, G, B, A]) ->
	Red = case catch list_to_integer(R) of
			  {'EXIT', _} -> list_to_float(R);
			  RedValue -> RedValue
		  end,
	Green = case catch list_to_integer(G) of
				{'EXIT', _} -> list_to_float(G);
				GreenValue -> GreenValue
			end,
	Blue = case catch list_to_integer(B) of
			   {'EXIT', _} -> list_to_float(B);
			   BlueValue -> BlueValue
		   end,
	Alpha = case catch list_to_integer(A) of
				{'EXIT', _} -> list_to_float(A);
				AlphaValue -> AlphaValue
			end,
	{Red, Green, Blue, Alpha}.

