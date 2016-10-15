-module(erad_acceptor).

-behaviour(supervisor).

%% API functions
-export([accept/2]).
-export([start_link/1]).

-export([start_link/0]).

-export([init_it/1]).
%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod), {Mod, {Mod, start_link, []},
                     temporary, 5000, worker, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
accept({tcp, Socket}, []) ->
  {ok, Pid} = Reply = supervisor:start_child(?MODULE, [Socket]),
  gen_tcp:controlling_process(Socket, Pid),
  Pid ! accept,
  Reply.

start_link(Socket) ->
  proc_lib:start_link(?MODULE, init_it, [Socket]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(?MODULE)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_it(Socket) when is_port(Socket) ->
  proc_lib:init_ack({ok, self()}),
  receive
    accept ->
      do_accept(Socket)
  after 5000 ->
      {timeout, accept}
  end.

do_accept(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"GET /", Path/binary>>} ->
      [Playlist | _] = binary:split(Path, <<" ">>),
      lager:debug("trying to connect to ~ts", [Playlist]),
      erad_connections_sup:accept(Socket, Playlist),
      normal
  after 5000 ->
      {timeout, request}
  end.
