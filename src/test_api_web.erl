
%% @doc Web server for test_api.

-module('test_api_web').
-include("test_api.hrl").

-export([loop/2, start/1, stop/0]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> (?MODULE):loop(Req, DocRoot) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop}
			 | Options1]).

stop() -> mochiweb_http:stop(?MODULE).

%% OTP 21 is the first to define OTP_RELEASE and the first to support
%% EEP-0047 direct stack trace capture.
-ifdef(OTP_RELEASE).

-if((?OTP_RELEASE) >= 21).

-define(HAS_DIRECT_STACKTRACE, true).

-endif.

-endif.

-ifdef(HAS_DIRECT_STACKTRACE).

- define ( CAPTURE_EXC_PRE ( Type , What , Trace ) , Type : What : Trace ) .


-define(CAPTURE_EXC_GET(Trace), Trace).

-else.

-define(CAPTURE_EXC_PRE(Type, What, Trace), Type:What).

-define(CAPTURE_EXC_GET(Trace),
	erlang:get_stacktrace()).

-endif.

loop(Req, DocRoot) ->
        "/" ++ Path = mochiweb_request:get(path, Req),
    ?H("Path: ~p", [Path]),
    try case re:split(Path, [$/], [{return, list}, {parts, 2}]) of
            [] ->
                error;
            [ControllerName, Rest] ->
                ControllerString = string:to_lower(ControllerName) ++ "_controller",
                Controller = test_api_util:list_to_atom(ControllerString),
                ?H("Controller: ~p", [Controller]),
                Method = mochiweb_request:get(method, Req),
                case Method of
                    ?GET ->
                        try case re:split(Rest, "\\?", [{return, list}, {parts, 2}]) of
                                [Rest] ->
                                    ActionString = string:to_lower(Rest),
                                    Action = test_api_util:list_to_atom(ActionString),
                                    Param = [],
                                    ?H("Controller: ~p, Action: ~p, Param: ~p", [Controller, Action, Param]),
                                    Controller:Action(Method, [], Req);
                                [ActionName, Param] ->
                                    ActionString = string:to_lower(ActionName),
                                    Action = test_api_util:list_to_atom(ActionString),
                                    ?H("Controller: ~p, Action: ~p, Param: ~p", [Controller, Action, Param]),
                                    Controller:Action(?GET, Param, Req);
                                _Else ->
                                    mochiweb_request:respond({500, [{"Content-Type", "text/plain"}], "request failed, sorry\n"}, Req)
                            end
                        catch _ ->
                            mochiweb_request:respond({500, [{"Content-Type", "text/plain"}], "request failed, sorry\n"}, Req)
                        end;
                    ?POST ->
                        ActionString = string:to_lower(Rest),
                        Action = test_api_util:list_to_atom(ActionString),
                        ?H("Action: ~p", [Action]),
                        Body = mochiweb_request:recv_body(Req),
                        ?H("Body: ~p", [Body]),
                        (Controller):(Action)(?POST, Body, Req)
                end;
            _Else ->
                error
        end
    catch ?CAPTURE_EXC_PRE(Type, What, Trace) ->
        Report = ["web request failed", {path, Path}, {type, Type}, {what, What}, {trace, ?CAPTURE_EXC_GET(Trace)}],
        error_logger:error_report(Report),
        mochiweb_request:respond({500, [{"Content-Type", "text/plain"}], "request failed, sorry\n"}, Req)
    end.

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options),
     proplists:delete(Option, Options)}.

create_response(ResponseType, Data) ->
    case ResponseType of
        "text/plain" ->
            ReqContentType = {"Content-Type", "text/plain"},
            Resp = {200, [ReqContentType], Data},
            io:fwrite("~nResp : ~p~n", [Resp]),
            Resp;
        "text/xml" ->
            ReqContentType = {"Content-Type", "text/xml"},
            SoapData = "<ROOT><TOP><VERSION>1.0</VERSION><SOURCE>0</SOURCE><CUST_ID>00001029129222</CUST_ID><REQUEST_TIME>2015-01-02 12:01:01</REQUEST_TIME></TOP><BODY><RSPCOD>00000</RSPCOD><RSPMSG>Success</RSPMSG></BODY></ROOT>",
            Resp = {200, [ReqContentType], SoapData},
            io:fwrite("~nResp : ~p~n", [Resp]),
            Resp;
        "application/json" ->
            %Ud = {struct, [{"api_res", {struct, [{"code", "0"}, {"description", "success"}, {"data" , "hey, hey.."}]}}]},
            Ud = {struct, [{"code", "0"}, {"description", "success"}, {"data" , Data}]},
		    Ud1 = iolist_to_binary(mochijson:encode(Ud)),
		    Resp = {200, [{"Content-Type", "application/json"}], [Ud1]},
            io:fwrite("~nResp : ~p~n", [Resp]),
            Resp
    end.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual("No, but I will!",
		 "Have you written any tests?"),
    ok.

-endif.
