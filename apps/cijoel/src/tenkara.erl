-module (tenkara).
-compile(export_all).

static_path(RequestPath) ->
  lists:concat(["site/public", RequestPath]).

init() ->
  Routes = dict:new(),
  Routes1 = dict:store('GET', [], Routes),
  dict:store('POST', [], Routes1).

get(App, Path, Handler) ->
  http_method(App, 'GET', Path, Handler).

post(App, Path, Handler) ->
  http_method(App, 'POST', Path, Handler).

http_method(App, Method, Path, Handler) ->
  Routes = dict:fetch(Method, App),
  NewRoutes = lists:append(Routes, [{Path, Handler}]),
  dict:store(Method, NewRoutes, App).

handle_static_request(RequestPath) ->
  {static, RequestPath}.

handle_dynamic_request(RequestMethod, RequestPath, App) ->
  Routes = dict:fetch(RequestMethod, App),
  MatchRoute = match_route(RequestPath),
  case lists:filter(MatchRoute, Routes) of
    [] -> handle_static_request(RequestPath);
    [{_, Handler}|_] -> Handler()
  end.

match_route(RequestPath) ->
  fun({Pattern, _}) ->
    case re:run(RequestPath, Pattern) of
      nomatch -> false;
      {match, _} -> true
    end
  end.

call(App, Request) ->
  RequestPath = Request:path(),
  StaticPath = static_path(RequestPath),
  StaticPathExists = filelib:is_regular(StaticPath),
  if
    StaticPathExists ->
      handle_static_request(RequestPath);
    true ->
      RequestMethod = Request:request_method(),
      handle_dynamic_request(RequestMethod, RequestPath, App)
  end.