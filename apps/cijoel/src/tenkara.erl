-module(tenkara).
-export ([
  init/1,
  terminate/2,
  handle_call/3,
  code_change/3,
  handle_cast/2,
  handle_info/2,
  get/2,
  post/2,
  handle/1
]).
-behaviour(gen_server).

% public
get(Path, Handler) ->
  append('GET', Path, Handler).

post(Path, Handler) ->
  append('POST', Path, Handler).

handle(Request) ->
  route(Request:request_method(), RequestPath).

% private
append(Method, Path, Handler) ->
  gen_server:call(?MODULE, {Method, Path, Handler});
append(Method, Path, Handler, RouteSet) ->
  Routes = dict:fetch(Method, RouteSet),
  NewRoutes = lists:append(Routes, [{Path, Handler}]),
  dict:store(Method, NewRoutes, RouteSet).

find(RequestPath, Routes)
  lists:filter(match(RequestPath), Routes).

match(RequestPath) ->
  fun({Pattern, _}) ->
    case re:run(RequestPath, Pattern) of
      nomatch -> false;
      {match, _} -> true
    end
  end.

route(RequestMethod, RequestPath) ->
  gen_server:call(?MODULE, {RequestMethod, RequestPath});
route(RequestMethod, RequestPath, RouteSet) ->
  Routes = dict:fetch(RequestMethod, RouteSet),
  case find(RequestPath, Routes) of
    [{_, Handler}|_] -> {dynamic, Handler(RequestPath)};
    _ -> {static, RequestPath}
  end.

% gen_server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  RouteSet = dict:new(),
  RouteSet1 = dict:store('GET', [], RouteSet),
  {ok, dict:store('POST', [], RouteSet1)}.

handle_call({Method, Path, Handler}, _, RouteSet) ->
  NewRouteSet = append(Method, Path, Handler, RouteSet),
  {reply, ok, NewRouteSet};
handle_call({RequestMethod, RequestPath}, _, RouteSet) ->
  Response = route(RequestMethod, RequestPath, RouteSet),
  {reply, Response, RouteSet}.

code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.

% RequestPath = Request:path(),
% StaticPath = lists:concat(["site/public", RequestPath]),
% StaticPathExists = filelib:is_regular(StaticPath),
% if
%   StaticPathExists -> route(static, RequestPath);
%   true -> route(dynamic, Request:request_method(), RequestPath)
% end;