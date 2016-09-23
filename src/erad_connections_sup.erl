-module(erad_connections_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).
-export([listen/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Port), {Port, {erad_connection, listen, [Port]},
                      transient, 5000, supervisor, [erad_connection]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

listen(Port) ->
  supervisor:start_child(?MODULE, ?CHILD(Port)).

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
  get_childs(filelib:wildcard(PrivDir ++ "/lib/*"), iolist_to_binary([PrivDir, "/lib/"]), []).

get_childs([Dir | Dirs], Prefix, Acc) ->
  case re:run(Dir, iolist_to_binary(["^", Prefix, "(\\d+)$"]), [{capture, all_but_first, binary}]) of
    {match, [Port]} ->
      get_childs(Dirs, Prefix, [?CHILD(binary_to_integer(Port)) | Acc]);
    _ ->
      get_childs(Dirs, Prefix, Acc)
  end;
get_childs([], _Prefix, Acc) ->
  Acc.
