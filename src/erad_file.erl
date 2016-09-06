-module(erad_file).

-export([open/1]).
-export([read/2]).

open(Filename) ->
  case file:open(Filename, ['read', 'raw', 'binary']) of
    {'ok', File} -> {'ok', {?MODULE, File}};
    Else -> Else
  end.

read(File, Len) ->
  file:read(File, Len).
