
-define(H(), {current_function, {Module, Function, _}} = process_info(self(), current_function),
	io:format("~n~nHarshaka >> {~p, ~p, ~p}~n~n", [Module, Function, ?LINE])).
-define(H(X, Y), {current_function, {Module, Function, _}} = process_info(self(), current_function),
	io:format("~n~nHarshaka >> {~p, ~p, ~p} -->~n" ++ X ++ "~n~n", [Module, Function, ?LINE] ++ Y)).

-define(GET, 'GET').
-define(POST, 'POST').

-define(APPLICATION, test_api).