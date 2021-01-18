-module(gleam@function_test).
-compile(no_auto_import).

-export([compose_test/0, curry2_test/0, curry3_test/0, curry4_test/0, curry5_test/0, curry6_test/0, flip_test/0, identity_test/0, rescue_test/0]).

compose_test() ->
    Add_two = fun(Int) -> Int + 2 end,
    Add_three = fun(Int@1) -> Int@1 + 3 end,
    Add_five = gleam@function:compose(Add_two, Add_three),
    gleam@should:equal(Add_five(1), 6),
    Head_to_string = gleam@function:compose(
        gleam@function:compose(
            fun gleam@list:head/1,
            fun(Gleam@capture_variable) ->
                gleam@result:unwrap(Gleam@capture_variable, 0)
            end
        ),
        fun gleam@int:to_string/1
    ),
    gleam@should:equal(Head_to_string([1]), <<"1"/utf8>>),
    gleam@should:equal(Head_to_string([]), <<"0"/utf8>>).

curry2_test() ->
    Fun = fun(A, B) -> A + B end,
    Curried = gleam@function:curry2(Fun),
    gleam@should:equal((Curried(1))(2), 3).

curry3_test() ->
    Fun = fun(A, B, C) -> (A + B) + C end,
    Curried = gleam@function:curry3(Fun),
    gleam@should:equal(((Curried(1))(2))(4), 7).

curry4_test() ->
    Fun = fun(A, B, C, D) -> ((A + B) + C) + D end,
    Curried = gleam@function:curry4(Fun),
    gleam@should:equal((((Curried(1))(2))(4))(8), 15).

curry5_test() ->
    Fun = fun(A, B, C, D, E) -> (((A + B) + C) + D) + E end,
    Curried = gleam@function:curry5(Fun),
    gleam@should:equal(((((Curried(1))(2))(4))(8))(16), 31).

curry6_test() ->
    Fun = fun(A, B, C, D, E, F) -> ((((A + B) + C) + D) + E) + F end,
    Curried = gleam@function:curry6(Fun),
    gleam@should:equal((((((Curried(1))(2))(4))(8))(16))(32), 63).

flip_test() ->
    Fun = fun(S, I) ->
        gleam@string:append(
            gleam@string:append(
                gleam@string:append(
                    gleam@string:append(<<"String: '"/utf8>>, S),
                    <<"', Int: '"/utf8>>
                ),
                gleam@int:to_string(I)
            ),
            <<"'"/utf8>>
        )
    end,
    Flipped_fun = gleam@function:flip(Fun),
    gleam@should:equal(
        Fun(<<"Bob"/utf8>>, 1),
        <<"String: 'Bob', Int: '1'"/utf8>>
    ),
    gleam@should:equal(
        Flipped_fun(2, <<"Alice"/utf8>>),
        <<"String: 'Alice', Int: '2'"/utf8>>
    ).

identity_test() ->
    gleam@should:equal(gleam@function:identity(1), 1),
    gleam@should:equal(gleam@function:identity(<<""/utf8>>), <<""/utf8>>),
    gleam@should:equal(gleam@function:identity([]), []),
    gleam@should:equal(gleam@function:identity({1, 2.0}), {1, 2.0}).

rescue_test() ->
    gleam@should:equal(gleam@function:rescue(fun() -> 1 end), {ok, 1}),
    gleam@should:equal(
        gleam@function:rescue(fun() -> erlang:throw(1) end),
        {error, {thrown, gleam@dynamic:from(1)}}
    ),
    gleam@should:equal(
        gleam@function:rescue(fun() -> erlang:error(<<""/utf8>>) end),
        {error, {errored, gleam@dynamic:from(<<""/utf8>>)}}
    ).
