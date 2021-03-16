# Pattern matching #

In last section we did some recursive functions. Now we will try to do some pattern matching and later combine it with recursive functions.

When it comes to Erlang we execute from right to left that makes it easier to understand the error message.

```erlang
2> Variable = 1.
1
3> Variable = 1.
1
4> 1 = Variable.
1
5> a = Variable.
** exception error: no match of right hand side value 1
```

This is a Erlang shell that we bind Variable to 1 and then pattern match.

At 3>, we try to pattern match 1 to Variable, if Variable haven't been set before it will be 1. But now it is set at 2>. At 4>, we try to match Variable to 1, and if it is ok the value is returned. At 5> we have a mismatch, this is where we match things from right to left. So on the right hand side we try to match 1 to a. That fails because of a no match.

One other example we can do is fibonacci. Fibonacci works that it do addition on the last number and the number before. 1, 1, 2, 3, 5, 8, 13.

We implement this in Erlang using recursion and pattern matching.

```erlang
-module(fibonacci).

-export([fib/1]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).
```

What does our code do?

We have two base case here, if N is 0 and if N is 1. In the last function clause we call fib function with N-1 and N-2.

## maps, records, tuples

Say that we want to do different things depending on a value in a map.

We get a list with maps that looks something like:
```erlang
[#{type => sms, text => <<"hi">>}, #{type => <<"email">>, text => <<"hi">>}]
```

We want to sum the amount of each type in our list.

```erlang
-module(pattern).

-export([sum_by_type/1,
         generate_list/0]).

sum_by_type(List) ->
    sum_by_type_aux(List, []).

sum_by_type_aux([], Acc) ->
    Acc;
sum_by_type_aux([#{type := Type} | Tail], Acc) ->
    case get_value(Acc, Type) of
        undefined ->
            sum_by_type_aux(Tail, [{Type, 1} | Acc]);
        Value ->
            NewAcc = remove_value(Acc, Type),
            sum_by_type_aux(Tail, [{Type, Value + 1} | NewAcc])
    end.

get_value([], _) ->
    undefined;
get_value([{Type, Value}|_], Type) ->
    Value;
get_value([_ | Tail], Type) ->
    get_value(Tail, Type).

remove_value(List, Type) ->
    remove_value_aux(List, Type, []).

remove_value_aux([], _, Acc) ->
    Acc;
remove_value_aux([{Type, _} | Tail], Type, Acc) ->
    remove_value_aux(Tail, Type, Acc);
remove_value_aux([Head | Tail], Type, Acc) ->
    remove_value_aux(Tail, Type, [Head | Acc]).

generate_list() ->
    [#{type => sms, text => <<"hi">>},
     #{type => email, text => <<"hi">>},
     #{type => sms, text => <<"hi">>},
     #{type => email, text => <<"hi">>},
     #{type => sms, text => <<"hi">>},
     #{type => sms, text => <<"hi">>},
     #{type => sms, text => <<"hi">>}].
```

Here is the code that will handle the sum and sort things. We create one function that will be the exported and that function will call a helper function with two arguments. First is the input we get, the other argument is the accumulator that we will return.

Helper function will match first element in the list and then check if we have already summed it. I have created my own functions now to get values and remove values to more show how pattern matching and recursion works. OTP library have proplists module or other modules to do this.

```erlang
4> List = pattern:generate_list().
[#{text => <<"hi">>,type => sms},
 #{text => <<"hi">>,type => email},
 #{text => <<"hi">>,type => sms},
 #{text => <<"hi">>,type => email},
 #{text => <<"hi">>,type => sms},
 #{text => <<"hi">>,type => sms},
 #{text => <<"hi">>,type => sms}]
5> pattern:sum_by_type(List).
[{sms,5},{email,2}]
```

Small examples on how to use lists, tuples and maps.