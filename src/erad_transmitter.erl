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
  lager:debug("starting"),
  start_link(["/home/data/music/alarm.mp3"]).

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

handle_info({frame, #{duration := Duration}, Frame}, #state{sockets = Sockets} = State) ->
  lager:debug("sending frame to sockets"),
  [gen_tcp:send(S, Frame) || S <- Sockets],
  timer:sleep(Duration),
  NewState = next_frame(State),
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

next_frame(#state{file = File} = State) when File /= undefined ->
  case erad_mp3:read_frame(File) of
    {frame, _Info, _Frame} = Frame ->
      self() ! Frame,
      State;
    eof ->
      erad_mp3:close(File),
      next_frame(State#state{file = undefined});
    _ ->
      next_frame(State)
  end;
next_frame(State) ->
  case get_next_file(State) of
    {ok, Filename, NewState} ->
      case erad_mp3:open(Filename) of
        {ok, File} ->
          next_frame(NewState#state{file = File});
        _ ->
          lager:error("cannot open file ~s", [Filename]),
          next_frame(NewState)
      end;
    {error, Reason, NewState} ->
      lager:alert("cannot get next file ~p", [Reason]),
      NewState
  end.

get_next_file(#state{playlist = [File | Rest]} = State) ->
  {ok, File, State#state{playlist = Rest ++ [File]}};
get_next_file(#state{playlist = []} = State) ->
  {error, <<"playlist is empty">>, State}.
