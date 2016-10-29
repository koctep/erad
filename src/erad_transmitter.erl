-module(erad_transmitter).

-behaviour(gen_server).

-export([accept/2]).
-export([start_link/1]).
-export([next/1]).
-export([playlist/1]).
-export([add/2]).
-export([play/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {name, sockets = [], playlist = [], file}).

accept(Pid, Socket) when is_pid(Pid) andalso is_port(Socket) ->
  lager:debug("setting controlling process ~p", [Pid]),
  ok = gen_tcp:controlling_process(Socket, Pid),
  gen_server:cast(Pid, {accept, Socket}),
  {ok, Pid}.

start_link([_ | _] = Name) ->
  LibDir = erad_util:lib_dir(),
  {ok, Contents} = file:read_file(LibDir ++ "/" ++ Name ++ ".m3u"),
  Files = re:split(re:replace(Contents, "\r\n", "", [global]), "\n", [{return, binary}]),
  Playlist = lists:map(maybe_add_dir(LibDir), [unicode:characters_to_list(File) || File <- Files, File /= <<>>]),
  gen_server:start_link(?MODULE, [Name, Playlist], []);
start_link(<<_/binary>> = Name) ->
  start_link(binary_to_list(Name)).

next([_ | _] = Playlist) ->
  gen_server:cast({via, erad_connections_sup, Playlist}, {next}).

playlist([_ | _] = Playlist) ->
  gen_server:call({via, erad_connections_sup, Playlist}, {playlist});
playlist(<<_/binary>> = Playlist) ->
  playlist(unicode:characters_to_list(Playlist)).

play([_ | _] = Playlist, No) ->
  gen_server:cast({via, erad_connections_sup, Playlist}, {play, No});
play(<<_/binary>> = Playlist, No) ->
  play(unicode:characters_to_list(Playlist), No).

add([_ | _ ] = Playlist, File) ->
  gen_server:cast({via, erad_connections_sup, Playlist}, {add, File}).

init([Name, Playlist]) ->
  lager:debug("initializing"),
  State = next_frame(#state{name = Name, playlist = Playlist}),
  {ok, State}.

handle_call({playlist}, _From, #state{playlist = Playlist} = State) ->
  {reply, Playlist, State};
handle_call(Msg, From, State) ->
  lager:warning("~p sent unhandled call ~p", [From, Msg]),
  {reply, {error, unhandled}, State}.

handle_cast({next}, #state{file = File} = State) ->
  erad_mp3:close(File),
  {noreply, State#state{file = undefined}};
handle_cast({accept, Socket}, #state{sockets = Sockets} = State) ->
  lager:debug("accepting socket"),
  inet:setopts(Socket, [{active, true}]),
  lager:debug("sending"),
  gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\n"
                         "Content-Type: audio/mpeg\r\n"
                         "Connection: close\r\n"
                         "Pragma: no-cache\r\n"
                         "Cache-Control: no-cache, no-store\r\n"
                         "\r\n"
                       >>
              ),
  {noreply, State#state{sockets = [Socket | Sockets]}};
handle_cast({add, File}, #state{playlist = Playlist} = State) ->
  {noreply, State#state{playlist = Playlist ++ [File]}};
handle_cast({play, No}, #state{file = File, playlist = Playlist} = State)
    when No > 0 andalso No =< length(Playlist) ->
  Filename = lists:nth(No, Playlist),
  lager:info("playing ~ts", [Filename]),
  case erad_mp3:open(Filename) of
    {ok, NewFile} ->
      erad_mp3:close(File),
      {noreply, State#state{file = NewFile}};
    _Error ->
      lager:error("cannot open file ~ts", [Filename]),
      {noreply, State}
  end;
handle_cast(Msg, State) ->
  lager:warning("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({frame, _Info, Frame}, #state{sockets = Sockets} = State) ->
  NewState = next_frame(State),
  lager:debug("sending frame to sockets"),
  [gen_tcp:send(S, Frame) || S <- Sockets],
  {noreply, NewState};
handle_info({frames, Frames}, #state{sockets = Sockets} = State) ->
  NewState = next_frame(State),
  [gen_tcp:send(S, Frames) || S <- Sockets],
  {noreply, NewState};
handle_info({tcp, _Socket, Data}, State) ->
  [lager:debug("~s", [X]) || X <- binary:split(Data, [<<"\r\n">>, <<"\n">>])],
  {noreply, State};
handle_info({tcp_closed, Socket}, #state{sockets = Sockets} = State) ->
  lager:info("connection closed"),
  {noreply, State#state{sockets = Sockets -- [Socket]}};
handle_info(Msg, State) ->
  lager:warning("unhandled info ~p", [Msg]),
  {noreply, State}.

terminate(Reason, #state{sockets = Sockets}) ->
  [gen_tcp:close(Socket) || Socket <- Sockets],
  lager:debug("terminating reason ~p", [Reason]),
  'ok'.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

next_frame(State) ->
  next_frame(State, [], 0, 5).

next_frame(State, Frames, Duration, N) when N < 0 ->
  erlang:send_after(Duration, self(), {frames, lists:reverse(Frames)}),
  State;
next_frame(#state{file = File} = State, Frames, Duration, N) when File /= undefined ->
  case erad_mp3:read_frame(File) of
    {frame, #{duration := FrameDuration} = _Info, Frame} ->
      next_frame(State, [Frame | Frames], Duration + FrameDuration, N - 1);
    eof ->
      erad_mp3:close(File),
      timer:sleep(3000),
      next_frame(State#state{file = undefined}, Frames, Duration, N);
    _ ->
      next_frame(State, Frames, Duration, N)
  end;
next_frame(State, Frames, Duration, N) ->
  case get_next_file(State) of
    {ok, Filename, NewState} ->
      lager:info("playing ~ts", [Filename]),
      case erad_mp3:open(Filename) of
        {ok, File} ->
          next_frame(NewState#state{file = File}, Frames, Duration, N);
        _Error ->
          lager:error("cannot open file ~ts: ~p", [Filename, _Error]),
          next_frame(NewState, Frames, Duration, N)
      end;
    {error, Reason, NewState} ->
      lager:alert("cannot get next file ~p", [Reason]),
      NewState
  end.

get_next_file(#state{playlist = Playlist} = State) ->
  lager:debug("getting next file"),
  case erad_strategy:next(Playlist) of
    {ok, File, NewPlaylist} ->
      lager:debug("next file is ~ts", [File]),
      {ok, File, State#state{playlist = NewPlaylist}};
    _Error ->
      lager:error("failed get next file: ~p", [_Error]),
      _Error
  end.

maybe_add_dir(Dir) ->
  fun([$/ | _ ] = File) -> File;
    (File) -> unicode:characters_to_list(Dir ++ "/" ++ File)
  end.
