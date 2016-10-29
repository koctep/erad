-module(erad_strategy).

-export([next/1]).

next([First | _] = Playlist) ->
  {ok, First, Playlist};
next([]) ->
  {error, <<"empty list">>}.
