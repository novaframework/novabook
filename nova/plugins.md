# Plugins #

Plugins in Nova is modules that have a behaviour. These behaviours will be a part of the pipeline flow of a request.



```erlang
-module(nova_correlation_plugin).
-behaviour(nova_plugin).

-export([pre_http_request/2,
         post_http_request/2,
         plugin_info/0]).

pre_http_request(#{req := Req} = NovaState, _) ->
    UUID = uuid:uuid_to_string(uuid:get_v4()),
    Req1 = cowboy_req:set_resp_header(<<"x-correlation-id">>, UUID, Req),
    NewState = maps:put(req, Req1, NovaState),
    {ok, NewState}.

post_http_request(NovaState, _) ->
    {ok, NovaState}.

plugin_info() ->
    {<<"nova_cors_plugin">>, <<"0.1.0">>, <<"">>, <<"Add CORS headers to request">>, []}.

```

This is an example plugin that will add a correlation id to all your request.

## Pipeline

We can look at the flow of a request as a pipeline that will go into different plugins before and after it have done controller code.

Plugins can be used on both http and websockets. The example above show two funcitons that the plugin have, pre_http_request (things that is before the controller) and post_http_request (things that is after the controller).

In the routing module we did show that you can use a security module to authenticate endpoints.

In Nova we have a security plugin  that will check if security is set or not.

[Nova Security Plugin](https://github.com/novaframework/nova/blob/master/src/nova_security_plugin.erl)

### Prioritization & Configuration

We want to say in what order we want plugins to be run. This we will do in sys.config where we also can add optionsal settings to plugin.

```erlang
 {nova, [
         {cowboy_configuration, #{
                                  port => 8190
                                 }},
         {dev_mode, true},
         {bootstrap_application, MYAPP}, %% Bootstraps the application
         %% Plugins is written on form {RequestType, Module, Options, Priority}
         %% Priority is that the lowest number is executed first
         {plugins, [
                    {pre_http_request, nova_security_plugin, #{}, 2},
                    {pre_http_request, nova_cors_plugin, #{}, 0},
                    {pre_http_request, nova_request_plugin, #{decode_json_body => true,
                                                              parse_bindings => true,
                                                              parse_qs => true}, 10},
                    {pre_ws_upgrade, nova_security_plugin, #{}, 20}
                   ]}
        ]},
```
Here we see that we have configured three plugins for http and one for websocket. For http we have the last number in the tuple to say what order to run things. Lowest number runs first.

In this case:
nova_cors_plugin with prio at 0
nova_security_plugin with prio at 2
nova_request_plugin with prio at 10

Websocket only have security plugin so it will only run that.

In the nova_request_plugin we have set some values:
```erlang
{pre_http_request, nova_request_plugin, #{decode_json_body => true,
                                                              parse_bindings => true,
                                                              parse_qs => true}, 10}
```
What will request plugin do?
What it will do is that it will move bindings from cowboy request to nova state. That the state that will be in the function header will look like this:
```erlang
#{req => CowboyReq,
  bindings => Bindings} (Same as in CowboyReq, but you don't need to think of Req)
```

Then we have decode_json_body this will if we get a body in the  request and it it content-type: application/json decode the body and move it to Nova state.

```erlang
#{req => CowboyReq,
  bindings => Bindings,
  json => JSONMap} (Keys in JSONMap will be binary)
```

The last part of the request plugin will add QS to Nova state if QS is used.

```erlang
#{req => CowboyReq,
  bindings => Bindings,
  json => JSONMap,
  qs => QS}
```

Because we have this in a plugin this will happen for all request that is used for this Nova application. So we don't need to handle json decode for all of our controllers.

Plugins is a way to remove things that you would need to do in all you requests to a place where it is handled by the system for you.