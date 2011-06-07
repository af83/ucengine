-module(unicode_helpers).

-export([normalize_unicode/1]).

% Convert strange unicode character like e' to é
normalize_unicode(Input) -> unicode:characters_to_list(binary:list_to_bin(Input), unicode).
