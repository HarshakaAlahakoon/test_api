%%%-------------------------------------------------------------------
%%% @author harshaka
%%% @copyright (C) 2020, harshaka
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2020 3:01 PM
%%%-------------------------------------------------------------------
-module(hello_controller).
-author("harshaka").
-include("test_api.hrl").
-compile(export_all).

%% API

hello(?GET, Param) ->
    ?H(),
    ok.
