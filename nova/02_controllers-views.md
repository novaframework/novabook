## Controllers & Views ##

Controllers and Views are the VC part of  the framework. Nova haven't got the model part yet for data but it have been discussed.

### Views ###
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

### Controllers ###

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    {ok, [{message, "Nova is running!"}]}.

```

If you change the message it will also be changed on the site.