## Adding auth and different views ##

In previous section we created a Nova application.

Now we will add a small login form and try to auth and if we pass it will show a view with message "Welcome Daniel!".

The structure we have in applications using nova is that in src/ we usually have modules that is used by our application. In the directory src/controllers we will have erlang modules that will be used to handle requests.
In the directory src/views we have the .dtl files for our endpoints. The names need to match so MY_VIEW.dtl should match MY_VIEW_controller.erl.

Security is handling in our routing file. It looks like this.

```erlang
#{prefix => "",
  security => false,
  routes => [{"/", { my_first_nova_main_controller, index}, #{methods => [get]}}]
 }.
```
Nova has the possibiltity to have different routes depending on what you want to achieve. So for now we want to add a setting for endpoints that will use a security module.

```erlang
#{prefix => "",
  security => false,
  routes => [{"/", { my_first_nova_main_controller, index}, #{methods => [get]}}]
 }.
#{prefix => "",
  security => {my_first_nova_auth, auth},
  routes => [
	     {"/login", { my_first_nova_login_controller, index}, #{methods => [post]}}
           ]
}.
```

When we add the input form it will submit username and password on the endpoint `/login`. If it pass our auth module my_first_nova_auth it will show my_first_nova_login.dtl and also my_first_nova_login_controller that will have the logic.

First we change the view for our main page my_first_nova_main.dtl.

```html
<html>
<body>
  <div>
    <form action="/login" method="post" id="nameform">
      <label for="username">userame:</label>
      <input type="text" id="username" name="username"><br>
      <label for="password">Password:</label>
      <input type="password" id="password" name="password"><br>
      <input type="submit" value="submit">
    </form>
  </div>
</body>
</html>
```
We have added an input form now that have username and password. When we click on the submit button it will trigger the auth module.

my_first_nova_auth.erl, we specified in our routing file what the module should be called and the function that will be used.
```erlang
-module(my_first_nova_auth).

-export([auth/1]).

auth(Req) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    {Username, _} = ParsedData = parse_input(Data),
    case ParsedData of
	      {<<"daniel">>, <<"test">>} ->
	          {true, #{<<"username">> => Username,
		                 <<"authed">> => true}};
	      _ ->
	          {true, #{<<"authed">> => false}}
    end.

parse_input(Data) ->
    [_, Username, _, Password] = bstring:tokens(Data, <<"=&">>),
    {Username, Password}.
```
Input will send data as a string so we will need to parse it to get the data. Nova uses cowboy as a web server and it have many functionality how to get headers or body. In this module we use `cowboy_req:read_body/1`.

Data here will be a binary string `<<"username=USERNAME&password=PASSWORD">>`.

bstring is a module that works in the same way as strings in erlang OTP library. Nova have some libraries inbuilt like [jhn_stdlib](https://github.com/JanHenryNystrom/jhn_stdlib).

When we return a tuple with {true, map()}, the map will be passed to the controller as a second argument. If we hada rest api or want to send back a 401 to the one that did the request we return false in our auth module. But in this case we wan't to redirect back to `/` if you pass in wrong credentials. If we did enter correct password and username we are going to show `Welcome USERNAME!`.

We need to create the controller now and the view for this, `my_first_nova_login_controller.erl` in `src/controllers/`.

```erlang
-module(my_first_nova_login_controller).

-export([index/1]).

index(#{<<"authed">> := true,
	      <<"username">> := Username}) ->
    {ok, [{message, <<"Welcome ", Username/binary, "!">>}]};
index(Req, #{<<"authed">> := false}) ->
    {redirect, "/"}.

```
In the controller we have a index function that take two arguments. First one is a cowboy req the other is nova state.

And then the view `my_first_nova_login.dtl` should be created in `src/views/`
```html
<html>
<body>
<h1> {{ message }} </h1>
</body>
</html>
```

What will happen here is that if we have the correct username and password the auth module will pass on the map we are giving in it. If `authed`is false it will redirect back to `/` if it returned true we should print the Username.

In this case we didn't use any database or so to have users, just to show how things works. If you want to see this with what you enter in the username we can change the auth module to.
```erlang
-module(my_first_nova_auth).

-export([auth/1]).

auth(Req) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    case parse_input(Data) of
	{Username, <<"test">>} ->
	    {true, #{<<"username">> => Username,
		     <<"authed">> => true}};
	_ ->
	    {true, #{<<"authed">> => false}}
    end.

parse_input(Data) ->
    [_, Username, _, Password] = bstring:tokens(Data, <<"=&">>),
    {Username, Password}.
```

In this case Username will be passed on to the controller that will print it on the page.

---
