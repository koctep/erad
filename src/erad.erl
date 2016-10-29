-module(erad).

-behaviour(application).
-behaviour(supervisor).

-export([start/2]).
-export([stop/1]).
-export([start_link/0]).
-export([init/1]).

-define(CHILD(Id, Mod, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, worker, [Mod]}).

start(_StartType, _StartArgs) ->
  lager:debug("starting app"),
  start_link().

stop(_State) ->
  'ok'.

start_link() ->
  lager:debug("starting sup"),
  supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

init([]) ->
  Port = get_port(),
  SupFlags = {rest_for_one, 0, 1},
  Childs = [?CHILD(files, erad_files, []),
            ?CHILD(connections, erad_connections_sup, []),
            ?CHILD(listener, tcp_terminator, [{tcp, erad_acceptor, Port, {0, 0, 0, 0}, []}]),
            ?CHILD(acceptor, erad_acceptor, []),
            ?CHILD(rest, erad_cow, [])
           ],
  lager:debug("initializing sup"),
  {'ok', {SupFlags, Childs}}.

get_port() ->
  case os:getenv("ERAD_PORT") of
    false ->
      {ok, Port} = application:get_env(port),
      Port;
    PortStr ->
      list_to_integer(PortStr)
  end.
