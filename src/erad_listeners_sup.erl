-module(erad_listeners_sup).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Id = {tcp, 8005},
  Childs = [{Id,
             {tcp_terminator, start_link, [{tcp, erad_transmitters_sup, 8005, {0, 0, 0, 0}, []}]},
             transient,
             5000,
             worker,
             []
            }],
  {ok, {{one_for_one, 0, 1}, Childs}}.
