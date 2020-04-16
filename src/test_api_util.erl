%%%-------------------------------------------------------------------
%%% @author harshaka
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2020 6:01 PM
%%%-------------------------------------------------------------------
-module(test_api_util).
-author("harshaka").
-include("test_api.hrl").
-compile(export_all).

list_to_atom(String) ->
    case erlang:list_to_existing_atom(String) of
        badarg ->
            erlang:list_to_atom(String);
        Atom ->
            Atom
    end.


