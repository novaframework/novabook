## Recursive functions ##

This is me trying to describe recursive functions in Erlang. How you can write them and how it works.

Erlang uses recursive functions often because in other languages you have for, while and other loops. In Erlang we do it with recursive functions.

Example:
```erlang
-module(recursive).

-export([sum/1]).

sum([]) ->
    0;
sum([Head | Tail]) ->
   Head + sum(Tail).
```

Here we do a sum function that will take a list of integers and summarize them.

```bash
15> recursive:sum([1,2,3]).
6
```

What happen when we call our function?

Thing in Erlang works that we go from the top to the bottom of our code.

recursive:sum([1,2,3]). When we call this function with argument [1,2,3] this will happen.

```erlang
sum([]) ->
   0;
sum([1 | [2,3]]) ->
   1 + sum([2,3]).
```

First we will hit the first function clause and because our list contain three elements it will go on to the next function clause. Here the head of the list will be 1 and the tail will be list with two elements [2,3].

The recursive call that we call our function sum again but now with the tail will give us.

```erlang
sum([]) ->
   0;
sum([2 | [3]]) ->
   2 + sum([3]).
```

We also do that with the last element in the list.

```erlang
sum([]) ->
   0;
sum([3 | []]) ->
   3 + sum([]).
```

Last call here will hit the base case and return a 0.

This will calculate 3 + 2 + 1 + 0 (base case) = 6.

You can also do this with tail recursive so you pass the result of the recursive to the next.

```erlang
sum([], Sum) ->
    Sum;
sum([Head | Tail], Sum) ->
    sum(Tail, Sum + Head).
```

This is our function with tail recursive. What will happen here is very similar to what we did above. Different now is that we to the addition in the recursive step and send the result to the next recursive. When we hit base case we will return the result.

```erlang
16> c(recursive).
{ok,recursive}
17> recursive:sum([1,2,3], 0).
6
```

We have now created a function called sum that now have aritry 2. It will take the list with integers and the start value.

```erlang
sum([], Sum) ->
   Sum;
sum([1 | [2,3]], 0) ->
   sum([2,3], 0 + 1).
```

```erlang
sum([], Sum) ->
   Sum;
sum([2 | [3]], 1) ->
   sum([3], 1 + 2).
```

```erlang
sum([], Sum) ->
   Sum;
sum([3 | []], 3) ->
   sum([], 3 + 3).
```

```erlang
sum([], 6) ->
   6;
sum([Head | Tail], Sum) ->
   sum(Tail, Sum + Head).
```

When we hit the base case and the list is empty we will return the variable Sum that is in this case 6.

---
