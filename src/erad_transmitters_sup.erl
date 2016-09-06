-module(erad_transmitters_sup).

-export([accept/2]).

-export([start_link/0]).
-export([init/1]).

accept({tcp, Socket}, Opts) ->
  gen_tcp:controlling_process(Socket, erlang:whereis(?MODULE)),
  supervisor:start_child(?MODULE, [{tcp, Socket}, Opts]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = {simple_one_for_one, 0, 1},
  Childs = [{'_',
             {erad_transmitter, accept, []},
             temporary,
             brutal_kill,
             worker,
             []
            }],
  {ok, {SupFlags, Childs}}.
