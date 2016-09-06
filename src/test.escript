#!/usr/bin/env escript

%%! -pa ../ebin -pa ../deps/_/ebin

-export([main/1]).

main([]) ->
  io:format("no filename~n");
main(["-l ", Lib | Opts]) ->
  code:add_patha(Lib),
  main(Opts);
main(Filename) ->
  {ok, F} = erad_mp3:open(Filename),
  read(F).

read(F) ->
  case erad_mp3:read_frame(F) of
    {frame, Data} -> io:format("read frame ~s~n", ['_':to_hex(Data)]), read(F);
    eof -> io:format("end of file~n"), halt(0);
    _ -> io:format("read something else~n"), read(F)
  end.
