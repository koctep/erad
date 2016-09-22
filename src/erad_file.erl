-module(erad_file).

-export([open/1]).
-export([close/1]).
-export([read/2]).

open(Filename) ->
  case file:open(Filename, ['read', 'raw', 'binary']) of
    {'ok', File} -> {'ok', {?MODULE, File}};
    Else -> Else
  end.

close(File) ->
  file:close(File).

read(File, Len) ->
  file:read(File, Len).
