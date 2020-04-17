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
    case catch erlang:list_to_existing_atom(String) of
        {'EXIT', _} ->
            erlang:list_to_atom(String);
        Atom ->
            Atom
    end.

read_priv_file(Filename) ->
    case code:priv_dir(?APPLICATION) of
        {error, bad_name} ->
            PrivDir = "priv";
        PrivDir ->
            ok
    end,
    ?H("File name: ~p", [filename:join([PrivDir, Filename])]),
    file:read_file(filename:join([PrivDir, Filename])).