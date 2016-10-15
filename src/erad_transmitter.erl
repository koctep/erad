-module(erad_transmitter).

-behaviour(gen_server).

-export([accept/2]).
-export([start_link/0]).
-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {sockets = [], playlist = [], file}).

accept(Pid, {tcp, Socket}) ->
  lager:debug("setting controlling process ~p", [Pid]),
  ok = gen_tcp:controlling_process(Socket, Pid),
  gen_server:cast(Pid, {accept, Socket}),
  {ok, Pid}.

start_link() ->
  start_link([]).

start_link({playlist, Port}) when is_integer(Port) ->
  start_link({playlist, erlang:integer_to_list(Port)});
start_link({playlist, SubDir}) ->
  case code:priv_dir(erad) of
    {error, bad_name} ->
      {error, <<"priv dir not found">>};
    Dir ->
      Playlist = filelib:wildcard(Dir ++ "/lib/" ++ SubDir ++ "/*.mp3"),
      start_link(Playlist)
  end;

start_link(Playlist) ->
  gen_server:start_link(?MODULE, Playlist, []).

init(Playlist) ->
  lager:debug("initializing"),
  State = next_frame(#state{playlist = Playlist}),
  {ok, State}.

handle_call(Msg, From, State) ->
  lager:warning("~p sent unhandled call ~p", [From, Msg]),
  {reply, {error, unhandled}, State}.

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
  next_frame(State, [], 0, 100).

next_frame(State, Frames, Duration, N) when N < 0 ->
  erlang:send_after(Duration, self(), {frames, lists:reverse(Frames)}),
  State;
next_frame(#state{file = File} = State, Frames, Duration, N) when File /= undefined ->
  case erad_mp3:read_frame(File) of
    {frame, #{duration := FrameDuration} = _Info, Frame} ->
      next_frame(State, [Frame | Frames], Duration + FrameDuration, N - 1);
    eof ->
      erad_mp3:close(File),
      next_frame(State#state{file = undefined}, Frames, Duration, N);
    _ ->
      next_frame(State, Frames, Duration, N)
  end;
next_frame(State, Frames, Duration, N) ->
  case get_next_file(State) of
    {ok, Filename, NewState} ->
      case erad_mp3:open(Filename) of
        {ok, File} ->
          next_frame(NewState#state{file = File}, Frames, Duration, N);
        _ ->
          lager:error("cannot open file ~ts", [Filename]),
          next_frame(NewState, Frames, Duration, N)
      end;
    {error, Reason, NewState} ->
      lager:alert("cannot get next file ~p", [Reason]),
      NewState
  end.

get_next_file(#state{playlist = [File | Rest]} = State) ->
  lager:info("starting ~ts", [File]),
  NewPlaylist = Rest ++ [File],
  {ok, File, State#state{playlist = NewPlaylist}};
get_next_file(#state{playlist = []} = State) ->
  {error, <<"playlist is empty">>, State}.
