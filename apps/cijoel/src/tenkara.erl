-module(tenkara).
-compile(export_all).
-export ([code_change/3, init/1, handle_call/3, terminate/2, handle_cast/2, handle_info/2]).
-behaviour(gen_server).

code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  RouteSet = dict:new(),
  RouteSet1 = dict:store('GET', [], RouteSet),
  {ok, dict:store('POST', [], RouteSet1)}.

get(Path, Handler) ->
  append('GET', Path, Handler).

post(Path, Handler) ->
  append('POST', Path, Handler).

append(Method, Path, Handler) ->
  gen_server:call(?MODULE, {Method, Path, Handler}).

handle_call({Method, Path, Handler}, _, RouteSet) ->
  Routes = dict:fetch(Method, RouteSet),
  NewRoutes = lists:append(Routes, [{Path, Handler}]),
  NewRouteSet = dict:store(Method, NewRoutes, RouteSet),
  {reply, ok, NewRouteSet};

handle_call({RequestMethod, RequestPath}, _, RouteSet) ->
  Response = route(dynamic, RequestMethod, RequestPath, RouteSet),
  {reply, Response, RouteSet}.

find(RequestPath, Routes)
  lists:filter(match(RequestPath), Routes).

match(RequestPath) ->
  fun({Pattern, _}) ->
    case re:run(RequestPath, Pattern) of
      nomatch -> false;
      {match, _} -> true
    end
  end.

route(Request) ->
  RequestPath = Request:path(),
  StaticPath = lists:concat(["site/public", RequestPath]),
  StaticPathExists = filelib:is_regular(StaticPath),
  if
    StaticPathExists -> route(static, RequestPath);
    true -> route(dynamic, Request:request_method(), RequestPath)
  end;
route(static, RequestPath) ->
  {static, RequestPath};
route(dynamic, RequestMethod, RequestPath) ->
  gen_server:call(?MODULE, {RequestMethod, RequestPath});
route(dynamic, RequestMethod, RequestPath, RouteSet) ->
  Routes = dict:fetch(RequestMethod, RouteSet),
  case find(RequestPath, Routes) of
    [{_, Handler}|_] -> {dynamic, Handler(RequestPath)};
    _ -> route(static, RequestPath)
  end.
