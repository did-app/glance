-module(gleam@set_test).
-compile(no_auto_import).

-export([size_test/0, contains_test/0, delete_test/0, to_list_test/0, from_list_test/0, fold_test/0, filter_test/0, take_test/0, union_test/0, intersection_test/0]).

size_test() ->
    gleam@should:equal(gleam@set:size(gleam@set:new()), 0),
    gleam@should:equal(
        gleam@set:size(
            gleam@set:insert(gleam@set:insert(gleam@set:new(), 1), 2)
        ),
        2
    ),
    gleam@should:equal(
        gleam@set:size(
            gleam@set:insert(
                gleam@set:insert(gleam@set:insert(gleam@set:new(), 1), 1),
                2
            )
        ),
        2
    ).

contains_test() ->
    gleam@should:be_true(
        gleam@set:contains(gleam@set:insert(gleam@set:new(), 1), 1)
    ),
    gleam@should:be_false(gleam@set:contains(gleam@set:new(), 1)).

delete_test() ->
    gleam@should:be_false(
        gleam@set:contains(
            gleam@set:delete(gleam@set:insert(gleam@set:new(), 1), 1),
            1
        )
    ).

to_list_test() ->
    gleam@should:equal(
        gleam@list:sort(
            gleam@set:to_list(
                gleam@set:insert(
                    gleam@set:insert(gleam@set:insert(gleam@set:new(), 2), 3),
                    4
                )
            ),
            fun gleam@int:compare/2
        ),
        [2, 3, 4]
    ).

from_list_test() ->
    gleam@should:equal(
        gleam@list:sort(
            gleam@set:to_list(gleam@set:from_list([1, 1, 2, 4, 3, 2])),
            fun gleam@int:compare/2
        ),
        [1, 2, 3, 4]
    ).

fold_test() ->
    gleam@set:fold(gleam@set:from_list([1, 3, 9]), 0, fun(M, A) -> M + A end).

filter_test() ->
    gleam@should:equal(
        gleam@set:to_list(
            gleam@set:filter(
                gleam@set:from_list([1, 4, 6, 3, 675, 44, 67]),
                fun gleam@int:is_even/1
            )
        ),
        [4, 6, 44]
    ).

take_test() ->
    gleam@should:equal(
        gleam@set:take(gleam@set:from_list([1, 2, 3]), [1, 3, 5]),
        gleam@set:from_list([1, 3])
    ).

union_test() ->
    gleam@should:equal(
        gleam@set:to_list(
            gleam@set:union(
                gleam@set:from_list([1, 2]),
                gleam@set:from_list([2, 3])
            )
        ),
        [1, 2, 3]
    ).

intersection_test() ->
    gleam@should:equal(
        gleam@set:to_list(
            gleam@set:intersection(
                gleam@set:from_list([1, 2]),
                gleam@set:from_list([2, 3])
            )
        ),
        [2]
    ).
