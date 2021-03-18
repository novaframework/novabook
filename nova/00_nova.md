# Nova #

## How to read this ##

### Part 1 ###

Here we will discuss foundation of Nova, how to use it as a view-controller.

### Part 2 ###

In this section we will show how to use Nova as a api server withour views. With routing and plugins.

### Part 3 ###

In this section we will show how to use js frameworks and static pages with Nova.

## Pre req ##

You will need erlang 22+ installed and rebar3.

## Installing Nova ##

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

---
