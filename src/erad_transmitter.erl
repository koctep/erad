-module(erad_transmitter).

-behaviour(gen_server).

-export([accept/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {socket, opts}).

accept({tcp, Socket}, Opts) ->
  lager:debug("accepting socket"),
  {ok, Pid} = Reply = gen_server:start_link(?MODULE, [Socket, Opts], []),
  gen_tcp:controlling_process(Socket, Pid),
  gen_server:cast(Pid, accept),
  Reply.

init([Socket, Opts]) ->
  lager:debug("initializing"),
  {ok, #state{socket = Socket, opts =  Opts}}.

handle_call(Msg, From, State) ->
  lager:warning("~p sent unhandled call ~p", [From, Msg]),
  {reply, {error, unhandled}, State}.

handle_cast(accept, #state{socket = Socket} = State) ->
  lager:debug("appling options to socket"),
  inet:setopts(Socket, [{active, true}]),
  gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\n"
                         "Content-Type: audio/mpeg\r\n"
                         "Connection: close\r\n"
                         "Pragma: no-cache\r\n"
                         "Cache-Control: no-cache, no-store\r\n"
                         "\r\n"
                       >>
              ),
%  {ok, File} = erad_mp3:open("/home/data/music/alarm.mp3"),
%  send(Socket, File),
  {noreply, State};
%  {stop, eof, State};
handle_cast(Msg, State) ->
  lager:warning("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
  [lager:debug("~s", [X]) || X <- binary:split(Data, [<<"\r\n">>, <<"\n">>])],
  {noreply, State};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
  lager:info("connection closed"),
  {stop, normal, State};
handle_info(Msg, State) ->
  lager:warning("unhandled info ~p", [Msg]),
  {noreply, State}.

terminate(Reason, #state{socket = Socket} = State) ->
  gen_tcp:close(Socket),
  lager:debug("terminating reason ~p", [Reason]),
  'ok'.

code_change(_OldVsn, State, Extra) ->
  {ok, State}.

send(Socket, File) ->
  case erad_mp3:read_frame(File) of
    {frame, _Info, Frame} ->
      lager:debug("sending frame"),
      gen_tcp:send(Socket, Frame),
      send(Socket, File);
    eof ->
      'stop';
    _ ->
      send(Socket, File)
  end.
