# Routing #

Here I will write about routing in Nova and how it works.

When we generate a new nova app we will have a routing file in the directory priv/.

File is named MYAPP.routes.erl in that file we have a erlang map that will specify and configurate what rules is for a endpoint.

```erlang
#{prefix => "",
  security => false,
  routes => [{"/", { my_first_nova_login_controller, login}, #{methods => [get]}}]
 }.
```

What does this routing configuration say us?
Prefix is what we will match against first, in this way you can create different routing configures depending on prefix.

```erlang
#{prefix => "",
  security => false,
  routes => [{"/", { my_first_nova_login_controller, login}, #{methods => [post]}}]
 }.

#{prefix => "/user/:userid",
  security => {my_first_nova_auth, auth},
  routes => [
         {"/", { my_first_nova_user_controller, get_user}, #{methods => [get]}},
         {"/pet", {my_first_nova_user_controller, get_user_pet}, #{methods => [get]}}
           ]
}.
```
First map we have a if you go against http://BASEURL:PORT/, we have said we will not have any authentication module on this endpoint. When we reach this endpoint we will call my_first_nova_login_controller:login if method is post.

In the second map we have a prefix that will check that every request with path http://BASEURL:PORT/user/:userid will use this configuration. Here we say that we want to use a auth module, so Nova will use my_first_nova_auth:auth to authenticate the request. If it is ok it will use the module my_first_nova_user_controller:get_user if the request was a get. In this way you can group endpoints also have different auth modules depending on endpoint.

In Nova we can also have explicit routing with that is that an endpoint will go direct to a module and function.

```erlang
#{prefix => "",
  security => false,
  routes => [{"/user/:userid", { my_first_nova_user_controller, get_user}, #{methods => [get]}},
{"/user/:userid", { my_first_nova_user_controller, update_user}, #{methods => [put]}},
{"/user/:userid", { my_first_nova_user_controller, delete_user}, #{methods => [delete]}}]
 }.
```
This routing configuration will allow us to decide on method what we want to do at an endpoint. Novas routing is little different from cowboys routing. That is we can have multiple paths that are the same but methods are different.

One of the reasons we choosed to do this way was that we didn't want to match against methods in our controllers. It would be nice if we knew what we wanted to do in the controller depending on endpoint and method.

The routing file in the end will be a map on what paths goes to what module and functions.
