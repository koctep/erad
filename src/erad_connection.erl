-module(erad_connection).

-behaviour(supervisor).

%% API functions
-export([accept/2]).
-export([listen/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, worker, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

accept({tcp, Socket}, _Opts) ->
  case get_transmitter(Socket) of
    undefined ->
      lager:alert("transmitter not found"),
      {error, <<"no transmitter">>};
    Transmitter ->
      erad_transmitter:accept(Transmitter, {tcp, Socket})
  end.

listen(Port) ->
  supervisor:start_link(?MODULE, Port).

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
init(Port) ->
  Transmitter = ?CHILD(transmitter, erad_transmitter, [{playlist, Port}]),
  Listener = ?CHILD(listener, tcp_terminator, [{tcp, ?MODULE, Port, {0, 0, 0, 0}, []}]),
  {ok, {{rest_for_one, 5, 10},
        [Transmitter, Listener]
       }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_connection(Port) ->
  case
    [Pid || {Port_, Pid, _, _} <- supervisor:which_children(erad_connections_sup), Port == Port_]
  of
    [] -> 'undefined';
    [Pid] -> Pid
  end.

get_transmitter(undefined) -> undefined;
get_transmitter(Socket) when is_port(Socket) ->
  {ok, {_, Port}} = inet:sockname(Socket),
  get_transmitter(get_connection(Port));

get_transmitter(Pid) ->
  [_, {transmitter, TransmitterPid, _, _} | _] = supervisor:which_children(Pid),
  TransmitterPid.
