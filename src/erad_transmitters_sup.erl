-module(erad_transmitters_sup).

-export([accept/2]).

-export([start_link/0]).
-export([init/1]).

accept({tcp, Socket}, _Opts) ->
  {ok, {_, Port}} = inet:sockname(Socket),
  case [Pid  || {{tcp, Port_}, Pid, _, _} <- supervisor:which_children(?MODULE), Port_ == Port] of
    [Pid] ->
      erad_transmitter:accept(Pid, {tcp, Socket});
    _ ->
      lager:alert("no transmitter for port ~p", [Port]),
      {error, <<"no transmitter">>}
  end.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = {one_for_one, 1, 1000},
  Childs = [{{tcp, 8005},
             {erad_transmitter, start_link, []},
             transient,
             brutal_kill,
             worker,
             []
            }],
  {ok, {SupFlags, Childs}}.
