-module(gleam@queue_test).
-compile(no_auto_import).

-export([from_and_to_list_test/0, is_empty_test/0, length_test/0, push_back_test/0, push_front_test/0, push_test/0, pop_back_test/0, pop_back_after_push_back_test/0, pop_back_after_push_test/0, pop_back_empty_test/0, pop_front_test/0, pop_front_after_push_front_test/0, pop_front_after_push_test/0, pop_front_empty_test/0, reverse_test/0, is_equal_test/0, is_logically_equal_test/0]).

from_and_to_list_test() ->
    gleam@should:equal(gleam@queue:from_list([]), gleam@queue:new()),
    gleam@should:equal(
        gleam@queue:to_list(gleam@queue:from_list([1, 2, 3])),
        [1, 2, 3]
    ).

is_empty_test() ->
    gleam@should:be_true(gleam@queue:is_empty(gleam@queue:new())),
    gleam@should:be_false(
        gleam@queue:is_empty(gleam@queue:from_list([<<""/utf8>>]))
    ).

length_test() ->
    Test = fun(Input) ->
        gleam@should:equal(
            gleam@queue:length(gleam@queue:from_list(Input)),
            gleam@list:length(Input)
        )
    end,
    Test([]),
    Test([1]),
    Test([1, 2]),
    Test([1, 2, 1]),
    Test([1, 2, 1, 5, 2, 7, 2, 7, 8, 4, 545]).

push_back_test() ->
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:push_back(gleam@queue:from_list([1, 2]), 3)
        ),
        [1, 2, 3]
    ),
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:push_back(
                gleam@queue:push_back(
                    gleam@queue:push_back(gleam@queue:new(), 1),
                    2
                ),
                3
            )
        ),
        [1, 2, 3]
    ).

push_front_test() ->
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:push_front(
                gleam@queue:push_front(gleam@queue:from_list([2, 3]), 1),
                0
            )
        ),
        [0, 1, 2, 3]
    ).

push_test() ->
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:push_back(
                gleam@queue:push_front(
                    gleam@queue:push_back(
                        gleam@queue:push_front(gleam@queue:new(), <<"b"/utf8>>),
                        <<"x"/utf8>>
                    ),
                    <<"a"/utf8>>
                ),
                <<"y"/utf8>>
            )
        ),
        [<<"a"/utf8>>, <<"b"/utf8>>, <<"x"/utf8>>, <<"y"/utf8>>]
    ).

pop_back_test() ->
    {ok, Tup} = gleam@queue:pop_back(gleam@queue:from_list([1, 2, 3])),
    gleam@should:equal(gleam@pair:first(Tup), 3),
    gleam@should:be_true(
        gleam@queue:is_equal(
            gleam@pair:second(Tup),
            gleam@queue:from_list([1, 2])
        )
    ).

pop_back_after_push_back_test() ->
    {ok, Tup} = gleam@queue:pop_back(
        gleam@queue:push_back(
            gleam@queue:push_back(
                gleam@queue:push_back(gleam@queue:new(), 1),
                2
            ),
            3
        )
    ),
    gleam@should:equal(gleam@pair:first(Tup), 3).

pop_back_after_push_test() ->
    {ok, Tup} = gleam@queue:pop_back(
        gleam@queue:push_back(
            gleam@queue:push_front(
                gleam@queue:push_back(
                    gleam@queue:push_front(gleam@queue:new(), <<"b"/utf8>>),
                    <<"x"/utf8>>
                ),
                <<"a"/utf8>>
            ),
            <<"y"/utf8>>
        )
    ),
    gleam@should:equal(gleam@pair:first(Tup), <<"y"/utf8>>).

pop_back_empty_test() ->
    gleam@should:equal(
        gleam@queue:pop_back(gleam@queue:from_list([])),
        {error, nil}
    ).

pop_front_test() ->
    {ok, Tup} = gleam@queue:pop_front(gleam@queue:from_list([1, 2, 3])),
    gleam@should:equal(gleam@pair:first(Tup), 1),
    gleam@should:be_true(
        gleam@queue:is_equal(
            gleam@pair:second(Tup),
            gleam@queue:from_list([2, 3])
        )
    ).

pop_front_after_push_front_test() ->
    {ok, Tup} = gleam@queue:pop_front(
        gleam@queue:push_front(
            gleam@queue:push_front(
                gleam@queue:push_front(gleam@queue:new(), 3),
                2
            ),
            1
        )
    ),
    gleam@should:equal(gleam@pair:first(Tup), 1).

pop_front_after_push_test() ->
    {ok, Tup} = gleam@queue:pop_front(
        gleam@queue:push_front(
            gleam@queue:push_back(
                gleam@queue:push_front(gleam@queue:new(), <<"b"/utf8>>),
                <<"x"/utf8>>
            ),
            <<"a"/utf8>>
        )
    ),
    gleam@should:equal(gleam@pair:first(Tup), <<"a"/utf8>>).

pop_front_empty_test() ->
    gleam@should:equal(
        gleam@queue:pop_front(gleam@queue:from_list([])),
        {error, nil}
    ).

reverse_test() ->
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:reverse(gleam@queue:from_list([1, 2, 3]))
        ),
        [3, 2, 1]
    ),
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:reverse(
                gleam@queue:push_back(
                    gleam@queue:push_back(
                        gleam@queue:push_back(gleam@queue:new(), 1),
                        2
                    ),
                    3
                )
            )
        ),
        [3, 2, 1]
    ),
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:reverse(
                gleam@queue:push_front(
                    gleam@queue:push_front(
                        gleam@queue:push_front(gleam@queue:new(), 1),
                        2
                    ),
                    3
                )
            )
        ),
        [1, 2, 3]
    ),
    gleam@should:equal(
        gleam@queue:to_list(
            gleam@queue:reverse(
                gleam@queue:push_back(
                    gleam@queue:push_back(
                        gleam@queue:push_front(
                            gleam@queue:push_front(gleam@queue:new(), 1),
                            2
                        ),
                        3
                    ),
                    4
                )
            )
        ),
        [4, 3, 1, 2]
    ).

is_equal_test() ->
    Should_equal = fun(A, B) ->
        gleam@should:be_true(gleam@queue:is_equal(A, B))
    end,
    Should_not_equal = fun(A@1, B@1) ->
        gleam@should:be_false(gleam@queue:is_equal(A@1, B@1))
    end,
    Should_equal(gleam@queue:new(), gleam@queue:new()),
    Should_equal(
        gleam@queue:push_front(gleam@queue:new(), 1),
        gleam@queue:push_back(gleam@queue:new(), 1)
    ),
    Should_equal(
        gleam@queue:push_front(gleam@queue:new(), 1),
        gleam@queue:push_front(gleam@queue:new(), 1)
    ),
    Should_equal(
        gleam@queue:push_back(gleam@queue:push_back(gleam@queue:new(), 1), 2),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ),
    Should_not_equal(
        gleam@queue:push_back(gleam@queue:new(), 1),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ),
    Should_not_equal(
        gleam@queue:push_back(gleam@queue:push_back(gleam@queue:new(), 2), 1),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ).

is_logically_equal_test() ->
    Both_even_or_odd = fun(A, B) ->
        gleam@int:is_even(A)
        =:= gleam@int:is_even(B)
    end,
    Should_equal = fun(A@1, B@1) ->
        gleam@should:be_true(
            gleam@queue:is_logically_equal(A@1, B@1, Both_even_or_odd)
        )
    end,
    Should_not_equal = fun(A@2, B@2) ->
        gleam@should:be_false(
            gleam@queue:is_logically_equal(A@2, B@2, Both_even_or_odd)
        )
    end,
    Should_equal(gleam@queue:new(), gleam@queue:new()),
    Should_equal(
        gleam@queue:push_front(gleam@queue:new(), 3),
        gleam@queue:push_back(gleam@queue:new(), 1)
    ),
    Should_equal(
        gleam@queue:push_front(gleam@queue:new(), 4),
        gleam@queue:push_back(gleam@queue:new(), 2)
    ),
    Should_equal(
        gleam@queue:push_front(gleam@queue:new(), 3),
        gleam@queue:push_front(gleam@queue:new(), 1)
    ),
    Should_equal(
        gleam@queue:push_back(gleam@queue:push_back(gleam@queue:new(), 3), 4),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ),
    Should_not_equal(
        gleam@queue:push_back(gleam@queue:new(), 1),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ),
    Should_not_equal(
        gleam@queue:push_back(gleam@queue:push_back(gleam@queue:new(), 2), 1),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ),
    Should_not_equal(
        gleam@queue:push_back(gleam@queue:push_back(gleam@queue:new(), 4), 3),
        gleam@queue:push_front(gleam@queue:push_front(gleam@queue:new(), 2), 1)
    ).
