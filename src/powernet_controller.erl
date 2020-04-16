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
    SoapData = "<ROOT><TOP><VERSION>1.0</VERSION><SOURCE>0</SOURCE><CUST_ID>00001029129222</CUST_ID><REQUEST_TIME>2015-01-02 12:01:01</REQUEST_TIME></TOP><BODY><RSPCOD>00000</RSPCOD><RSPMSG>Success</RSPMSG></BODY></ROOT>",
    mochiweb_request:respond({200, [{"Content-Type", "text/xml"}], SoapData}, Request).