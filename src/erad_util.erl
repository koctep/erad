-module(erad_util).

-export([lib_dir/0]).

lib_dir() ->
  case os:getenv("ERAD_LIB") of
    false -> code:priv_dir(erad) ++ "/lib";
    Dir -> Dir
  end.
