-module(erad).

-behaviour(application).
-behaviour(supervisor).

-export([start/2]).
-export([stop/1]).
-export([start_link/0]).
-export([listen/1]).
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

listen(Port) ->
  erad_connections_sup:listen(Port).

init([]) ->
  SupFlags = {one_for_one, 0, 1},
  Childs = [?CHILD(connections, erad_connections_sup, [])
           ],
  lager:debug("initializing sup"),
  {'ok', {SupFlags, Childs}}.
