-module(request_handler).
-export ([do/1]).
	
do(Info) ->
  Request = build_request(Info),
	Data = route(Request:request_method(), Request:path()),
	build_response(Data, Info).

project_path(RequestPath) ->
  lists:concat(["./site", RequestPath]).

route(RequestMethod, RequestPath) ->
  ProjectPath = project_path(RequestPath),
  case RequestMethod of
    'GET' -> handle_get(ProjectPath);
    'POST' -> handle_post(ProjectPath)
  end.

handle_get(ProjectPath) ->
  lists:concat(["GET ", ProjectPath]).

handle_post(ProjectPath) ->
  lists:concat(["POST ", ProjectPath]).

build_request(Info) ->
  simple_bridge:make_request(inets_request_bridge, Info).

build_response(Data, Info) ->
  ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
  Response1 = ResponseBridge:status_code(200),
  Response2 = Response1:header("Content-Type", "text/html"),
  Response3 = Response2:data(Data),
  Response3:build_response().