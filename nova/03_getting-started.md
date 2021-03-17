## Getting Started ##

[Nova](https://github.com/novaframework/nova) is a web framework written in Erlang and is using Cowboy as a web server.

### Pre req ###

You will need erlang 22+ installed and rebar3.

### Installing Nova ###

Via curl
```
sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/nova/master/tools/install.sh)"
```
Via wget
```
sh -c "$(wget -O- https://raw.githubusercontent.com/novaframework/nova/master/tools/install.sh)"
```

What the installation scripts do is setting up the rebar3 templates that we have made so it is easier to start a Nova app.

Also it is possible to get the templates from rebar3_nova plugin, add rebar3_nova to ~/.config/rebar3/rebar.config

```erlang
{project_plugins, [rebar3_nova]}
```

When scripts are done or when the plugin is added you can write this to create a new nova app.

```
rebar3 new nova my_first_nova
```
You will see that the template setup the directory and configurations to get first app running.

```
===> Writing my_first_nova/config/sys.config
===> Writing my_first_nova/priv/my_first_nova.routes.erl
===> Writing my_first_nova/src/my_first_nova.app.src
===> Writing my_first_nova/src/my_first_nova_app.erl
===> Writing my_first_nova/src/my_first_nova_sup.erl
===> Writing my_first_nova/src/controllers/my_first_nova_main_controller.erl
===> Writing my_first_nova/rebar.config
===> Writing my_first_nova/src/views/my_first_nova_main.dtl
```
When this is installed you can run:
```
rebar3 shell
```

This will start my_first_nova application with a shell.
Now open a browser and go to `http://localhost:8080` you should see a page with the text `Nova is running!`

### What did we get? ###

Nova app have some configuration files, route file, controllers and views.

First the sys.config, this handle the my_first_nova application.
```erlang
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-

[
 {kernel, [
           {logger_level, debug}
          ]},
 {nova, [
         {cowboy_configuration, #{
                                  port => 8080
                                 }},
         {dev_mode, true},
         {bootstrap_application, my_first_nova}, %% Bootstraps the application
         {plugins, [
                   ]}
        ]}
  %% Please change your app.src-file instead if you intend to add app-specific configurations
].
```
What do we have here?
Well kernel section is for logging with logger, here we can later add different formatters or log handlers, this is basic Erlang configurations.

Nova section handles the nova application and when we start Nova in our app my_first_nova it knows what port and plugins it should use. Also what is the main app, the bootstrap_application section.

Plugins is a new feature that we are working with, but it is a way to have modules do som pre/post handling on your request.

Example:
> You want to get a user,for each request you want to have a correlation id. This can then be generated in a pre plugin that adds it to the header and pass it on. Then it can be used in the controller to keep track of what goes on.

Then we have the route file my_first_nova.routes.erl in this you will specify all endpoints or static assets that you want to expose.
```erlang
#{prefix => "",
  security => false,
  routes => [
            {"/", { my_first_nova_main_controller, index}, #{methods => [get]}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
```
Prefix is if you want to have something before the routes. Say "v1" then you can add it to prefix.

Security is if you want any way to auth the request. Change security to {Module, Function} instead of false if you want to have a auth module.

Routes is a tuple {PATH, { MODULE, FUNCTION }, OPTIONS}. If we break down the file above it will go to `http://localhost:8080/` and it will only allow method GET. When someone is doing a GET agains this it will use module my_first_nova_main_controller with function index.

This is the configurations that is for Nova.

### Controllers & Views ###

Each view have a controller for logic, the view is a Django template file.

Our view my_first_nova_main_view.dtl:
```html
<html>
<body>
<h1>{{message}}</h1>
</body>
</html>
```

Remember that we did see `Nova is running!` when you started the browser. `{{message}}` is something that will added from the controller with same name, my_first_nova_main_controller.erl.

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    {ok, [{message, "Nova is running!"}]}.

```

If you change the message it will also be changed on the site.

This was a short get started with Nova. I would be happy for any feedback what we can improve with Nova or guides. What you want to see more of.

Next [article](https://dev.to/taure/adding-auth-and-different-views-2hhl) is about Auth, routing and views.

---
