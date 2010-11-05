-module(translation).

-author('tbomandouki@af83.com').

-include("uce.hrl").

-define(GOOGLE_TRANSLATE_TIMEOUT, 4000).

-export([
	 start/1,
	 translate/2
	]).

start(_Options) ->
    inets:start(),
    triggers:add(#uce_trigger{location='_',
				type="post_annotation_event",
				action={{translation, translate}, [{"languages", ["en", "it"]}]}}),
    ok.

%% Translate based on google translator api
translate(Event, Params) ->
    case utils:get(Event#uce_event.metadata ++ Params,
		   ["text", "language", "languages"],
		   [none, none, none]) of
	[Text, Language, TrLanguages]
	  when is_list(Text) and is_list(Language) and is_list(TrLanguages) ->
	    DoTrad = fun(DestLanguage) ->
			     WebText = yaws_api:url_encode(Text),
			     URL = "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=" ++ WebText ++ "&langpair=" ++ Language ++ "|" ++ DestLanguage,
				 R = case catch httpc:request(get, {URL, []}, [{timeout, ?GOOGLE_TRANSLATE_TIMEOUT}], []) of
						 {'EXIT', _} -> http:request(get, {URL, []}, [{timeout, ?GOOGLE_TRANSLATE_TIMEOUT}], []);
						 _ = Good -> Good
					 end,
			     case R of
				 {ok, {{_, 200,"OK"}, _, Response}} ->
				     case mochijson:decode(Response) of
					 {struct, [{"responseData", {struct, [{"translatedText", Trad}]}}, _, _]} ->
					     event:add(#uce_event{
							  location=Event#uce_event.location,
							  from="translation",
							  type="translate_annotation_event",
							  metadata=[{"traduction", Trad},
								    {"language", DestLanguage}]});
					 _ ->
					     {error, bad_response}
				     end;
				 _ ->
				     {error, bad_response}
			     end
		     end,
	    [ DoTrad(Lang) || Lang <- TrLanguages],
	    ok;
	_ ->
	    {error, bad_parameters}
    end.
