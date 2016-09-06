-module(erad).

-behaviour(application).
-behaviour(supervisor).

-export([start/2]).
-export([stop/1]).
-export([start_link/0]).
-export([init/1]).

start(_StartType, _StartArgs) ->
  start_link().

stop(State) ->
  'ok'.

start_link() ->
  supervisor:start_link({'local', ?MODULE}, ?MODULE, [], []).

init([]) ->
  {'ok', {#{}, []}}.
