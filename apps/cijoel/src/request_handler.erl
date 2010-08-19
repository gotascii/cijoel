-module(request_handler).
-export ([do/1]).
	
do(Info) ->
  Request = build_request(Info),
	Data = generate_response_data(Request),
	build_response(Data, Info).

project_path(RequestPath) ->
  lists:concat(["site", RequestPath]).

static_path(RequestPath) ->
  lists:concat(["site/public", RequestPath]).

generate_response_data(Request) ->
  RequestPath = Request:path(),
  StaticPath = static_path(RequestPath),
  StaticPathExists = filelib:is_regular(StaticPath),
  if
    StaticPathExists ->
      handle_static(RequestPath);
    true ->
      RequestMethod = Request:request_method(),
      ProjectPath = project_path(RequestPath),
      case RequestMethod of
        'GET' -> handle_get(ProjectPath);
        'POST' -> handle_post(ProjectPath)
      end
  end.

handle_static(RequestPath) ->
  {static, RequestPath}.

handle_get(ProjectPath) ->
  {dynamic, lists:concat(["GET ", ProjectPath])}.

handle_post(ProjectPath) ->
  {dynamic, lists:concat(["POST ", ProjectPath])}.

build_request(Info) ->
  simple_bridge:make_request(inets_request_bridge, Info).

build_response({ResponseType, Data}, Info) ->
  Bridge = simple_bridge:make_response(inets_response_bridge, Info),
  Response = case ResponseType of
    static ->
      Bridge:file(Data);
    dynamic ->
      B1 = Bridge:status_code(200),
      B2 = B1:header("Content-Type", "text/html"),
      B2:data(Data)
  end,
  Response:build_response().