-module(example_client_test).
-compile(export_all).

-include("example.hrl").


store() -> 
    example_client:store(
        #contact{
            % Optional:
            id = 42,
            first_name = "?",
            last_name = "?",
            % List with zero or more elements:
            projects = ["?"]},
    _Soap_headers = [],
    _Soap_options = [{url,"http://localhost:8080"}]).

retrieve() -> 
    example_client:retrieve(
        #id{
            id = 42},
    _Soap_headers = [],
    _Soap_options = [{url,"http://localhost:8080"}]).

