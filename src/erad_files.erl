-module(erad_files).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).
-export([open/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {erad_file, start_link, []},
                                     temporary, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
open(File) ->
  supervisor:start_child(?MODULE, [File]).

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
  Child = {files,
           {erad_file, start_link, []},
           temporary,
           5000,
           worker,
           [erad_file]
          },
  {ok, {{simple_one_for_one, 5, 10}, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
