-module(request_handler).
-export ([do/1]).

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

do(Info) ->
  Request = build_request(Info),
  Data = tenkara:handle(Request),
	build_response(Data, Info).