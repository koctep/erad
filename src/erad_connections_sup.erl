-module(erad_connections_sup).

-behaviour(supervisor).

%% API functions
-export([accept/2]).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Dir), {Id, {erad_transmitter, start_link, [Dir]},
                      transient, 5000, supervisor, [erad_transmitter]}).

%%%===================================================================
%%% API functions
%%%===================================================================
accept(Socket, Playlist) ->
  [Pid] = [Pid || {Id, Pid, _, _} <- supervisor:which_children(?MODULE), Playlist =:= Id],
  erad_transmitter:accept(Pid, Socket).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
  Childs = get_childs(),
  {ok, {{one_for_one, 5, 10}, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_childs() ->
  PrivDir = code:priv_dir(erad),
  Dirs = filelib:wildcard(PrivDir ++ "/lib/*"),
  lists:map(fun(Dir) -> child_spec(PrivDir, Dir) end, Dirs).

child_spec(<<_/binary>> = PrivDir, <<_/binary>> = Playlist) ->
  L = byte_size(PrivDir),
  <<PrivDir:L/binary, "/lib/", Dir/binary>> = Playlist,
  ?CHILD(Dir, Playlist);
child_spec(PrivDir, Dir) ->
  child_spec(list_to_binary(PrivDir), list_to_binary(Dir)).
