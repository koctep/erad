-module(erad_rest).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([execute/2]).

init(_Type, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

allowed_methods(Req, Opts) ->
  {[<<"POST">>], Req, Opts}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, execute}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, execute}], Req, State}.

execute(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  case jsxn:decode(Body) of
    <<"playlists">> ->
      Playlists = [unicode:characters_to_binary(Name) || Name <- erad_connections_sup:playlists()],
      Req2 = cowboy_req:set_resp_body(jsxn:encode(Playlists), Req1),
      {true, Req2, State};
    #{<<"playlist">> := Playlist,
      <<"play">> := No
     } ->
      erad_transmitter:play(Playlist, No),
      {true, Req1, State};
    #{<<"playlist">> := Playlist} ->
      List = [unicode:characters_to_binary(Name) || Name <- erad_transmitter:playlist(Playlist)],
      Req2 = cowboy_req:set_resp_body(jsxn:encode(List), Req1),
      {true, Req2, State}
  end.
