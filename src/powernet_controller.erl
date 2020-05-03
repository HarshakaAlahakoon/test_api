%%%-------------------------------------------------------------------
%%% @author harshaka
%%% @copyright (C) 2020, harshaka
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2020 2:59 PM
%%%-------------------------------------------------------------------
-module(powernet_controller).
-author("harshaka").
-include("test_api.hrl").
-compile(export_all).

%% API

hello(?GET, Param, Request) ->
    ?H(),
    mochiweb_request:respond({200, [{"Content-Type", "text/plain"}], "Hello\n"}, Request).

qammeter(?POST, Body, Request) ->
    ?H(),
    {ok, <<XmlFile/binary>>} = test_api_util:read_priv_file("xml/qammeter_response.xml"),
    ?H("~p", [XmlFile]),
    mochiweb_request:respond({200, [{"Content-Type", "text/xml"}], XmlFile}, Request).

paypower(?POST, Body, Request) ->
    ?H(),
    {ok, <<XmlFile/binary>>} = test_api_util:read_priv_file("xml/paypower_response.xml"),
    ?H("~p", [XmlFile]),
    mochiweb_request:respond({200, [{"Content-Type", "text/xml"}], XmlFile}, Request).

accountpayinfo(?POST, Body, Request) ->
    ?H(),
    {ok, <<XmlFile/binary>>} = test_api_util:read_priv_file("xml/accountpayinfo_response.xml"),
    ?H("~p", [XmlFile]),
    mochiweb_request:respond({200, [{"Content-Type", "text/xml"}], XmlFile}, Request).

qagentppdetail(?POST, Body, Request) ->
    ?H(),
    {ok, <<XmlFile/binary>>} = test_api_util:read_priv_file("xml/qagentppdetail_response.xml"),
    ?H("~p", [XmlFile]),
    mochiweb_request:respond({200, [{"Content-Type", "text/xml"}], XmlFile}, Request).