## Testing ##

We want to build a pet store that have a api that handles pets in different ways.

Before we start to write Nova api for this it is good to start creating tests so we are sure that things are working when we build the api.

We will use Common test for this and to test with that it is much easier if we use two terminals. One that have the node running and the other that we run our tests in.

In first terminal:
```bash
rebar3 shell
```

This will start Nova and you will see a console when it is done.

In the second terminal we will run common tests with command:
```bash
rebar3 ct
```

### Test Suite ###

First we will create a new directory called `test` standing in root directory in you app.

In `./test` you craete a file called `pets_SUITE.erl` this will be our common test file that will keep our tests. What we want to test is to add a pet, get that pet, change name on pet and then remove the pet. Nova have a http client that can be used in tests (also in the code if want the node to do http requests).

This is a basic common test file:
```Erlang
-module(pets_SUITE).

-compile(export_all).

%% Includes
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

groups() -> [].

all() ->
    [].
```

What we will use here is init_per_suite and end_per_suite. Common test have more functionality that you can add, like init_per_group, end_per_group, init_per_testcase and end_per_testcase. All of this will initiate something before running the suite, group or testcase.

In init_per_suite I will have initial thing that will add the pet. The ID we get back I will store in the Config so the testcases that are in the Suite can use it. In end_per_suite I will remove the pet so we clean the data, end_per_suite will also be called if tests fails. This will remove test data that we add.
