-module(erad_file).

-behaviour(gen_server).

%% API functions
-export([start_link/1]).
-export([open/1]).
-export([close/1]).
-export([read/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


open(Filename) ->
  case erad_files:open(Filename) of
    {ok, Pid} ->
      lager:debug("opened ~p", [{?MODULE, Pid}]),
      {ok, {?MODULE, Pid}};
    Else -> Else
  end.

close(Pid) ->
  gen_server:cast(Pid, {close}).

read(Pid, Len) ->
  lager:debug("reading ~p bytes from ~p", [Len, Pid]),
  gen_server:call(Pid, {read, Len}).

-record(state, {file, buffer = <<>>}).

-define(BUFFER_SIZE, 65536).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Filename) ->
    gen_server:start_link(?MODULE, [Filename], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Filename]) ->
  case file:open(Filename, ['read', 'raw', 'binary']) of
    {'ok', File} ->
      {ok, Buffer} = file:read(File, ?BUFFER_SIZE * 2),
      {'ok', #state{file = File, buffer = Buffer}};
    Else -> {error, Else}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({read, _Len}, _From, #state{buffer = <<>>} = State) ->
  lager:debug("eof"),
  {reply, eof, State};
handle_call({read, Len}, From, #state{buffer = Buffer} = State)
    when Len > 0 andalso byte_size(Buffer) >= Len ->
  <<Data:Len/binary, Rest/binary>> = Buffer,
  gen_server:reply(From, {ok, Data}),
  lager:debug("read ~p bytes", [Len]),
  case State of
    #state{file = eof} ->
      lager:debug("eof"),
      {noreply, State#state{buffer = Rest}};
    #state{file = File} when byte_size(Rest) < ?BUFFER_SIZE ->
      lager:debug("getting addition"),
      case file:read(File, ?BUFFER_SIZE) of
        {ok, AddToBuffer} ->
          lager:debug("added"),
          {noreply, State#state{buffer = <<Rest/binary, AddToBuffer/binary>>}};
        eof ->
          lager:debug("eof"),
          file:close(File),
          {noreply, State#state{buffer = <<Rest/binary>>, file = eof}}
      end;
    _ ->
      lager:debug("all is, going futher"),
      {noreply, State#state{buffer = Rest}}
  end;
handle_call({read, _Len}, _From, #state{buffer = Data} = State) ->
  {reply, {ok, Data}, State#state{buffer = <<>>}};
handle_call(_Request, _From, State) ->
  lager:warning("unhandled request ~tp", [_Request]),
  {reply, {erron, unhandled}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({close}, #state{file = File} = State) ->
  case File of
    eof -> ok;
    File -> file:close(File)
  end,
  {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
