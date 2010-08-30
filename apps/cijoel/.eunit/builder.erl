-module (builder).
-export ([sha/1, build/1]).

get_result([Port|Output]) ->
  receive
    {Port, {data, Data}} ->
      get_result([Port, Data]);
    {Port, {exit_status, Status}} ->
      [Status|Output]
  end.

cmd([Cmd|Dir]) ->
  Opts = case Dir of
    [] -> [];
    _ -> [{cd, Dir}]
  end,
  Port = open_port({spawn, Cmd}, [exit_status|Opts]),
  get_result([Port]).

sha(ProjectPath) ->
  [_, Output] = cmd(["git rev-parse origin/master", ProjectPath]),
  lists:subtract(Output, "\n").

build(ProjectPath) ->
  spawn(fun() ->
    Result = cmd(["rake cruise", ProjectPath]),
    build_finished(Result)
  end).

build_finished([Status, _]) ->
  case Status of
    0 -> io:format("Success!");
    1 -> io:format("Failure!")
  end.