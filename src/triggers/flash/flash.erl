-module(flash).

-author('tbomandouki@af83.com').

-include("uce.hrl").

-define(FLASH_TIMEOUT, 4000).
-define(TYPE_EVENT, new_swf_file_generated).

%%
%% Mandatory packages :
%% swftools, a2ps, html2ps, imagemagick
%%

-export([
	 start/1,
	 generate/2
	]).

start(_Options) ->
    inets:start(),
    triggers:add(#uce_trigger{location='_',
				type="internal.file.add",
				action={{flash, generate}, []}}),
    ok.

generate(#uce_event{location=[Org, Meeting], metadata=Metadata}=Event, _Options) ->
	{value, {"filename", FileId}} = lists:keysearch("filename", 1, Metadata),
	FileRecord = file:get({Org, Meeting}, FileId),
	[_, _, Rep, FileName] = re:split(FileRecord#uce_file.uri, "/", [{return, list}]),
	BaseDir = config:get(datas) ++ "/" ++ Rep ++ "/",
	ContentType = yaws_api:mime_type(FileId),
	Cmd = case ContentType of
			  "application/pdf" -> "pdf2swf";
			  "image/jpeg" -> "jpeg2swf";
			  "image/gif" -> "gif2swf";
			  "image/png" -> "png2swf";
			  "video/avi" -> "avi2swf";
			  "audio/wav" -> "wav2swf";
			  "audio/x-wav" -> "wav2swf";
			  "text/plain" -> 
				  %% first : convert to ps
				  PsFile = BaseDir ++ FileName ++ ".ps",
				  PdfFile = BaseDir ++ FileName ++ ".pdf",
				  os:cmd("a2ps " ++ BaseDir ++ FileName ++ " -o " ++ PsFile),
				  %% next : convert ps file to pdf
				  os:cmd("ps2pdf " ++ PsFile ++ " " ++ PdfFile),
				  os:cmd("rm " ++ PsFile),
				  {"pdf2swf", PdfFile};
			  
			  _ -> none
		  end,
	case Cmd of
		none -> nothing;
		{Command, Pdf} -> 
			od:cmd(Command ++ " " ++ Pdf ++ " -o " ++ BaseDir ++ FileName ++ ".swf"),
			os:cmd("rm " ++ Pdf),
			file:add(#uce_file{id={{Org,Meeting}, FileId ++ ".swf"}, uri="file://" ++ Rep ++ "/" ++ FileName ++ ".swf"}),
			event:add(#uce_event{location=[Org, Meeting],
					       type=?TYPE_EVENT, 
					       from=Event#uce_event.from,
					       metadata=[{"source", FileName}, {"filename", FileId ++ ".swf"}]
					      });
	    _ ->
		os:cmd(Cmd ++ " " ++ BaseDir ++ FileName ++ " -o " ++ BaseDir ++ FileName ++ ".swf"),
		%% Add file and push new event
		file:add(#uce_file{id={{Org,Meeting}, FileId ++ ".swf"}, uri="file://" ++ Rep ++ "/" ++ FileName ++ ".swf"}),
		event:add(#uce_event{location=[Org, Meeting],
				       type=?TYPE_EVENT, 
				       from=Event#uce_event.from,
				       metadata=[{"source", FileName}, {"filename", FileId ++ ".swf"}]
				      })
	end,
	ok.
