## Getting Started ##

First we will generate a new Nova app that we will work with.

```Bash
rebar3 new nova user_management
```
You will see that the template setup the directory and configurations to get first app running.

```Bash
===> Writing user_management/config/dev_sys.config
===> Writing user_management/config/prod_sys.config
===> Writing user_management/priv/user_management.routes.erl
===> Writing user_management/src/user_management.app.src
===> Writing user_management/src/user_management_app.erl
===> Writing user_management/src/user_management_sup.erl
===> Writing user_management/src/controllers/user_management_main_controller.erl
===> Writing user_management/rebar.config
===> Writing user_management/config/vm.args
===> Writing user_management/src/views/user_management_main.dtl
```
When this is installed you can run:
```Bash
rebar3 shell
```

This will start user_management application with a shell.
Now open a browser and go to `http://localhost:8080` you should see a page with the text `Nova is running!`

### What did we get? ###

Nova app have some configuration files, route file, controllers and views.

First the sys.config, this handle the user_management application.
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
            {"/",{ my_first_nova_main_controller, index}, #{methods => [get]}}
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

---
