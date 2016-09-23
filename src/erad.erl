-module(erad).

-behaviour(application).
-behaviour(supervisor).

-export([start/2]).
-export([stop/1]).
-export([start_link/0]).
-export([init/1]).

start(_StartType, _StartArgs) ->
  lager:debug("starting app"),
  start_link().

stop(_State) ->
  'ok'.

start_link() ->
  lager:debug("starting sup"),
  supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = {one_for_one, 0, 1},
  Childs = [{listeners,
             {erad_listeners_sup, start_link, []},
             permanent,
             5000,
             supervisor,
             []
            },
            {transmitters,
             {erad_transmitters_sup, start_link, []},
             permanent,
             5000,
             supervisor,
             []
            }
           ],
  lager:debug("initializing sup"),
  {'ok', {SupFlags, Childs}}.
