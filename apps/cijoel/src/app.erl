-module(app).
-export ([init/0]).

project_path(RequestPath) ->
  lists:concat(["site", RequestPath]).

init() -> 
  tenkara:get("/jazz", fun(_) ->
    "Welcome to Jazz Town"
  end),
  tenkara:get(".*", fun(RequestPath) ->
    lists:concat(["GET ", project_path(RequestPath)])
  end),
  tenkara:post(".*", fun(RequestPath) ->
    lists:concat(["POST ", project_path(RequestPath)])
  end).