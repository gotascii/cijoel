-module (tenkara_tests).
-include_lib("eunit/include/eunit.hrl").

the_tenkara_module_test_() ->
  [
    { "should return path to the resource in the public folder",
      ?_assertEqual("site/public/resource", tenkara:static_path("/resource")) },

    { setup,
      fun() ->
        tenkara:init()
      end,
      fun(App) ->
        [
          { "should have a GET key that points to an empty list",
            ?_assertEqual([], dict:fetch('GET', App)) },

          { "should have a POST key that points to an empty list",
            ?_assertEqual([], dict:fetch('POST', App)) },

          { "should add a route entry to the list at the GET key",
            fun() -> 
              App1 = tenkara:get(App, "path", "handler"),
              Routes = dict:fetch('GET', App1),
              ?_assertEqual([{"path", "handler"}], Routes)
            end },

          { "should add a route entry to the list at the POST key",
            fun() -> 
              App1 = tenkara:post(App, "path", "handler"),
              Routes = dict:fetch('POST', App1),
              ?_assertEqual([{"path", "handler"}], Routes)
            end },

          { "should run a route handler that matches the path",
            fun() ->
              App1 = tenkara:get(App, "/path", fun() -> "handler!" end),
              Data = tenkara:handle_dynamic_request('GET', "/path", App1),
              ?assertEqual("handler!", Data)
            end },
          
          { "run a route handler that matches a regex path",
            fun() ->
              App1 = tenkara:get(App, "/weird*", fun() -> "handler!" end),
              Data = tenkara:handle_dynamic_request('GET', "/weirdness", App1),
              ?assertEqual("handler!", Data)
            end },
          
          { "run static handler if path is not matchable",
            fun() ->
              App1 = tenkara:get(App, "/weird_path", fun() -> "handler!" end),
              Data = tenkara:handle_dynamic_request('GET', "/path", App1),
              ?assertEqual({static, "/path"}, Data)
            end },

          { "run ",
            fun() ->
              App1 = tenkara:get(App, ".*", fun(RequestPath) ->
                lists:concat(["GET ", RequestPath])
              end),
              Data = tenkara:handle_dynamic_request('GET', "/path", App1),
              ?assertEqual("GET /path", Data)
            end }
        ]
      end }
  ].