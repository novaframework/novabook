## Controllers & Views ##

Controllers and Views are the VC part of  the framework. Nova haven't got the model part yet for data but it have been discussed.


In src we have a views directory that will contain all your views that will also have a controller with the same name in the directory controllers.

When we configure routing we will poing the module and function to the controller that will do some logic and then return {ok, Proplist}. When controller return ok tuple Nova will understand that it should render the view file and return it.

### Controllers ###

This is a basic controller that will return back a proplist back to the view. In this case it will return with a message that the view will populate.

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1
        ]).

index(#{method := <<"GET">>} = _Req) ->
    {ok, [{message, "Nova is running!"}]}.

```

If you change the message it will also be changed on the site.

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

From the controller we will get `Nova is running!`. That we will template into `{{message}}` that will show it on the homepage.

