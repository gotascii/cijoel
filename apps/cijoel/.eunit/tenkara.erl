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
    [{_, Handler}|_] ->
      Info = erlang:fun_info(Handler),
      {_, Arity} = lists:keyfind(arity, 1, Info),
      Data = case Arity of
        0 -> Handler();
        1 -> Handler(RequestPath)
      end,
      {dynamic, Data};
    _ -> handle_static_request(RequestPath)
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