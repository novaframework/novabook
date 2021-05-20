## Controllers & Views ##

Controllers and Views are the VC part of  the framework. Nova haven't got the model part yet for data but it have been discussed.


In src we have a views directory that will contain all your views that will also have a controller with the same name in the directory controllers.

When we configure routing we will poing the module and function to the controller that will do some logic and then return {ok, Proplist}. When controller return ok tuple Nova will understand that it should render the view file and return it.

### Controllers ###

This is a basic controller that will return back a proplist back to the view. In this case it will return with a message that the view will populate.

What do we have in this module?
---

***Erlang***
```erlang
%% This is the module declaration
-module(user_management_main_controller).
%% What functions that are exported from this module.
-export([
         index/1
        ]).
%% Function header.
index(_) ->
    %% Returns a tuple with ok and a proplists (Key-Value list)
    {ok, [{message, "Nova is running!"}]}.

```
---

***LFE***
```Lisp
(defmodule user_management_main_controller
  (export
   (index 1)))

(include-lib "logjam/include/logjam.hrl")

(defun index
  ;;
  ;; GET Handler
  ;;
  ((`#m(req #m(method #"GET")))
    `#(ok (#(message "nova is running!")))))
```
---

An Erlang module is structured first to have the module declaration to say that it is a module this is done with the first line `-module(Module)`. Comments are done with `%`, good rule is to use %% when comment on a line and % if you comment after code.

```Erlang
%% This is a comment
my_function() ->
    ok. % Comment after code
```

After this we have the export that will export functions outside the module so other can modules can use it or you can use it from a shell.

Then we have the index functions that in this take one argument.

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

From the controller we will get `Nova is running!`. That we will template into `{{message}}` that will show it on the homepage. What will happen here is that `{{ message }}` will be changed to the text that we get from our index function in section about the controller above. If we change the text it will also be added to the page.

