-module(erad_connections_sup).

-behaviour(supervisor).

%% API functions
-export([accept/2]).
-export([start_link/0]).
-export([whereis_name/1]).
-export([send/2]).
-export([playlists/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Name), {Name, {erad_transmitter, start_link, [Name]},
                      transient, 5000, supervisor, [erad_transmitter]}).

whereis_name(Playlist) ->
  [Pid] = [Pid || {Id, Pid, _, _} <- supervisor:which_children(?MODULE), Playlist =:= Id],
  Pid.

send(Playlist, Msg) ->
  whereis_name(Playlist) ! Msg.

playlists() ->
  [Id || {Id, _, _, _} <- supervisor:which_children(?MODULE)].
%%%===================================================================
%%% API functions
%%%===================================================================
accept(Socket, Playlist) ->
  erad_transmitter:accept(whereis_name(Playlist), Socket).

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
  LibDir = erad_util:lib_dir(),
  filelib:fold_files(LibDir, "^.*.m3u$", false, fold_child_spec(LibDir), []).

fold_child_spec(LibDir) ->
  fun(Playlist, Acc) ->
      {match, [Name]} = re:run(Playlist, LibDir ++ "/(.*)\.m3u$", [{capture, all_but_first, list}]),
      lager:info("playlist name ~s", [Name]),
      [?CHILD(Name) | Acc]
  end.
