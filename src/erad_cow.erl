-module(erad_cow).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).
-export([change_rules/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(REF, element(2, application:get_application())).
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
start_link() ->
  lager:debug("starting ~p", [?MODULE]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

change_rules() ->
  b5cow ! code_changed.

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
init([]) ->
  HttpEnv = '_':app_get_env(http_server, []),
  HttpPort = proplists:get_value(port, HttpEnv, 8001),
  {ok, State1} = maybe_start_http(HttpPort, []),
  State = State1,
  {ok, State}.

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
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
handle_info(code_changed, State) ->
  lager:debug("code changed, stopping"),
  {stop, code_changed, State};
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
terminate(_Reason, State) ->
  [cowboy:stop_listener({?REF, X}) || X <- State],
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
  self() ! code_changed,
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
maybe_start_http(undefined, State) ->
  {ok, State};
maybe_start_http(Port, State) ->
  lager:info("starting http server at ~p port", [Port]),
  {ok, _} = cowboy:start_http({?REF, http}, 1,
                              [{port, Port}, {ip, {0, 0, 0, 0}}],
                              [{env, [{dispatch, dispatch_rules()}]},
                               {middlewares, [cowboy_router, cowboy_handler]}
                              ]),
  {ok, [http | State]}.

dispatch_rules() ->
  {ok, AppName} = application:get_application(),
  cowboy_router:compile(
    [{'_', [{"/", cowboy_static, {priv_file, AppName, "static/index.html",
                                  [{mimetypes, w4c_mimetypes, web}]}}
            ,{"/api",  erad_rest, #{}}
           ]}]).
