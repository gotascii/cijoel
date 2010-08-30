-module(app).
-export ([init/0]).

project_path(RequestPath) ->
  lists:concat(["site", RequestPath]).

init() -> 
  App = tenkara:init(),
  App1 = tenkara:get(App, "/?", fun(RequestPath) ->
    lists:concat(["GET ", project_path(RequestPath)])
  end),
  tenkara:post(App1, "/?", fun(RequestPath) ->
    lists:concat(["POST ", project_path(RequestPath)])
  end).