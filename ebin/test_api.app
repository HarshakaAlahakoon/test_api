%% -*- erlang -*-
{application, test_api,
 [{description, "test_api"},
  {vsn, "0.1"},
  {modules, [test_api_app, test_api, test_api_deps, test_api_sup, test_api_web]},
  {registered, []},
  {mod, {test_api_app, []}},
  {env, [
      {http_ip, "127.0.0.1"}, 
      {http_port, 9090},
      {return_type, {true, "application/json"}}
      ]
  },
  {applications, [kernel, stdlib, crypto]}]}.
