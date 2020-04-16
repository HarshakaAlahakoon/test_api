%% @author Mochi Media <dev@mochimedia.com>
%% @copyright test_api Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the test_api application.

-module(test_api_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for test_api.
start(_Type, _StartArgs) ->
    test_api_deps:ensure(),
    test_api_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for test_api.
stop(_State) ->
    ok.
