-module(gleam@iterator_test).
-compile(no_auto_import).

-export([to_from_list_test/0, step_test/0, take_test/0, fold_test/0, map_test/0, flat_map_test/0, append_test/0, flatten_test/0, filter_test/0, repeat_test/0, cycle_test/0, unfold_test/0, range_test/0, drop_test/0, find_test/0]).

to_from_list_test() ->
    Test = fun(Subject) ->
        gleam@should:equal(
            gleam@iterator:to_list(gleam@iterator:from_list(Subject)),
            Subject
        )
    end,
    Test([]),
    Test([1]),
    Test([1, 2]),
    Test([1, 2, 4, 8]).

step_test() ->
    Test = fun(Subject) ->
        Step = gleam@iterator:step(gleam@iterator:from_list(Subject)),
        case Subject of
            [] ->
                gleam@should:equal(Step, done);

            [H | T] ->
                gleam@should:equal(Step, {next, H, gleam@iterator:from_list(T)})
        end
    end,
    Test([]),
    Test([1]),
    Test([1, 2]),
    Test([1, 2, 3]).

take_test() ->
    Test = fun(N, Subject) ->
        gleam@should:equal(
            gleam@iterator:take(gleam@iterator:from_list(Subject), N),
            gleam@list:take(Subject, N)
        )
    end,
    Test(0, []),
    Test(1, []),
    Test(-1, []),
    Test(0, [0]),
    Test(1, [0]),
    Test(-1, [0]),
    Test(0, [0, 1, 2, 3, 4]),
    Test(1, [0, 1, 2, 3, 4]),
    Test(2, [0, 1, 2, 3, 4]),
    Test(22, [0, 1, 2, 3, 4]).

fold_test() ->
    Test = fun(Subject, Acc, F) ->
        gleam@should:equal(
            gleam@iterator:fold(gleam@iterator:from_list(Subject), Acc, F),
            gleam@list:fold(Subject, Acc, F)
        )
    end,
    F@1 = fun(E, Acc@1) -> [E | Acc@1] end,
    Test([], [], F@1),
    Test([1], [], F@1),
    Test([1, 2, 3], [], F@1),
    Test([1, 2, 3, 4, 5, 6, 7, 8], [], F@1).

map_test() ->
    Test = fun(Subject, F) ->
        gleam@should:equal(
            gleam@iterator:to_list(
                gleam@iterator:map(gleam@iterator:from_list(Subject), F)
            ),
            gleam@list:map(Subject, F)
        )
    end,
    F@1 = fun(E) -> E * 2 end,
    Test([], F@1),
    Test([1], F@1),
    Test([1, 2, 3], F@1),
    Test([1, 2, 3, 4, 5, 6, 7, 8], F@1).

flat_map_test() ->
    Test = fun(Subject, F) ->
        gleam@should:equal(
            gleam@iterator:to_list(
                gleam@iterator:flat_map(gleam@iterator:from_list(Subject), F)
            ),
            gleam@list:flatten(
                gleam@list:map(
                    gleam@list:map(Subject, F),
                    fun gleam@iterator:to_list/1
                )
            )
        )
    end,
    F@1 = fun(I) -> gleam@iterator:range(I, I + 2) end,
    Test([], F@1),
    Test([1], F@1),
    Test([1, 2], F@1).

append_test() ->
    Test = fun(Left, Right) ->
        gleam@should:equal(
            gleam@iterator:to_list(
                gleam@iterator:append(
                    gleam@iterator:from_list(Left),
                    gleam@iterator:from_list(Right)
                )
            ),
            gleam@list:flatten([Left, Right])
        )
    end,
    Test([], []),
    Test([1], [2]),
    Test([1, 2], [3, 4]).

flatten_test() ->
    Test = fun(Lists) ->
        gleam@should:equal(
            gleam@iterator:to_list(
                gleam@iterator:flatten(
                    gleam@iterator:from_list(
                        gleam@list:map(Lists, fun gleam@iterator:from_list/1)
                    )
                )
            ),
            gleam@list:flatten(Lists)
        )
    end,
    Test([[], []]),
    Test([[1], [2]]),
    Test([[1, 2], [3, 4]]).

filter_test() ->
    Test = fun(Subject, F) ->
        gleam@should:equal(
            gleam@iterator:to_list(
                gleam@iterator:filter(gleam@iterator:from_list(Subject), F)
            ),
            gleam@list:filter(Subject, F)
        )
    end,
    Even = fun(X) -> (X rem 2) =:= 0 end,
    Test([], Even),
    Test([1], Even),
    Test([1, 2], Even),
    Test([1, 2, 3], Even),
    Test([1, 2, 3, 4], Even),
    Test([1, 2, 3, 4, 5], Even),
    Test([1, 2, 3, 4, 5, 6], Even).

repeat_test() ->
    gleam@should:equal(
        gleam@iterator:take(gleam@iterator:repeat(1), 5),
        [1, 1, 1, 1, 1]
    ).

cycle_test() ->
    gleam@should:equal(
        gleam@iterator:take(
            gleam@iterator:cycle(gleam@iterator:from_list([1, 2, 3])),
            9
        ),
        [1, 2, 3, 1, 2, 3, 1, 2, 3]
    ).

unfold_test() ->
    gleam@should:equal(
        gleam@iterator:take(
            gleam@iterator:unfold(2, fun(Acc) -> {next, Acc, Acc * 2} end),
            5
        ),
        [2, 4, 8, 16, 32]
    ),
    gleam@should:equal(
        gleam@iterator:take(gleam@iterator:unfold(2, fun(_) -> done end), 5),
        []
    ),
    gleam@should:equal(
        gleam@iterator:to_list(gleam@iterator:unfold(5, fun(N) -> case N of
                        0 ->
                            done;

                        N@1 ->
                            {next, N@1, N@1 - 1}
                    end end)),
        [5, 4, 3, 2, 1]
    ).

range_test() ->
    Test = fun(A, B, Expected) ->
        gleam@should:equal(
            gleam@iterator:to_list(gleam@iterator:range(A, B)),
            Expected
        )
    end,
    Test(0, 0, []),
    Test(1, 1, []),
    Test(-1, -1, []),
    Test(0, 1, [0]),
    Test(0, 5, [0, 1, 2, 3, 4]),
    Test(1, -5, [1, 0, -1, -2, -3, -4]).

drop_test() ->
    gleam@should:equal(
        gleam@iterator:to_list(
            gleam@iterator:drop(gleam@iterator:range(0, 10), 5)
        ),
        [5, 6, 7, 8, 9]
    ).

find_test() ->
    gleam@should:equal(
        gleam@iterator:find(gleam@iterator:range(0, 10), fun(E) -> E =:= 5 end),
        {ok, 5}
    ),
    gleam@should:equal(
        gleam@iterator:find(
            gleam@iterator:range(0, 10),
            fun(E@1) -> E@1 > 10 end
        ),
        {error, nil}
    ),
    gleam@should:equal(
        gleam@iterator:find(gleam@iterator:from_list([]), fun(_) -> true end),
        {error, nil}
    ),
    gleam@should:equal(
        gleam@iterator:find(
            gleam@iterator:unfold(
                {cat, 1},
                fun(Cat) -> {next, Cat, {cat, erlang:element(2, Cat) + 1}} end
            ),
            fun(Cat@1) -> erlang:element(2, Cat@1) =:= 10 end
        ),
        {ok, {cat, 10}}
    ).
