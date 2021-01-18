-module(gleam@list_test).
-compile(no_auto_import).

-export([length_test/0, reverse_test/0, is_empty_test/0, contains_test/0, head_test/0, tail_test/0, filter_test/0, filter_map_test/0, map_test/0, try_map_test/0, drop_test/0, take_test/0, new_test/0, append_test/0, flatten_test/0, fold_test/0, fold_right_test/0, find_map_test/0, find_test/0, all_test/0, any_test/0, zip_test/0, strict_zip_test/0, unzip_test/0, intersperse_test/0, at_test/0, unique_test/0, sort_test/0, index_map_test/0, range_test/0, repeat_test/0, split_test/0, split_while_test/0, key_find_test/0, pop_test/0, pop_map_test/0, key_pop_test/0, key_set_test/0, partition_test/0, permutations_test/0]).

length_test() ->
    gleam@should:equal(gleam@list:length([]), 0),
    gleam@should:equal(gleam@list:length([1]), 1),
    gleam@should:equal(gleam@list:length([1, 1]), 2),
    gleam@should:equal(gleam@list:length([1, 1, 1]), 3).

reverse_test() ->
    gleam@should:equal(gleam@list:reverse([]), []),
    gleam@should:equal(gleam@list:reverse([1, 2, 3, 4, 5]), [5, 4, 3, 2, 1]).

is_empty_test() ->
    gleam@should:be_true(gleam@list:is_empty([])),
    gleam@should:be_false(gleam@list:is_empty([1])).

contains_test() ->
    gleam@should:be_true(gleam@list:contains([0, 4, 5, 1], 1)),
    gleam@should:be_false(gleam@list:contains([0, 4, 5, 7], 1)),
    gleam@should:be_false(gleam@list:contains([], 1)).

head_test() ->
    gleam@should:equal(gleam@list:head([0, 4, 5, 7]), {ok, 0}),
    gleam@should:equal(gleam@list:head([]), {error, nil}).

tail_test() ->
    gleam@should:equal(gleam@list:tail([0, 4, 5, 7]), {ok, [4, 5, 7]}),
    gleam@should:equal(gleam@list:tail([0]), {ok, []}),
    gleam@should:equal(gleam@list:tail([]), {error, nil}).

filter_test() ->
    gleam@should:equal(gleam@list:filter([], fun(_) -> true end), []),
    gleam@should:equal(
        gleam@list:filter([0, 4, 5, 7, 3], fun(_) -> true end),
        [0, 4, 5, 7, 3]
    ),
    gleam@should:equal(
        gleam@list:filter([0, 4, 5, 7, 3], fun(X) -> X > 4 end),
        [5, 7]
    ),
    gleam@should:equal(
        gleam@list:filter([0, 4, 5, 7, 3], fun(X@1) -> X@1 < 4 end),
        [0, 3]
    ).

filter_map_test() ->
    gleam@should:equal(
        gleam@list:filter_map([2, 4, 6, 1], fun(X) -> {ok, X + 1} end),
        [3, 5, 7, 2]
    ),
    gleam@should:equal(
        gleam@list:filter_map([2, 4, 6, 1], fun(A) -> {error, A} end),
        []
    ).

map_test() ->
    gleam@should:equal(gleam@list:map([], fun(X) -> X * 2 end), []),
    gleam@should:equal(
        gleam@list:map([0, 4, 5, 7, 3], fun(X@1) -> X@1 * 2 end),
        [0, 8, 10, 14, 6]
    ).

try_map_test() ->
    Fun = fun(X) -> case ((X =:= 6) orelse (X =:= 5)) orelse (X =:= 4) of
            true ->
                {ok, X * 2};

            false ->
                {error, X}
        end end,
    gleam@should:equal(
        gleam@list:try_map([5, 6, 5, 6], Fun),
        {ok, [10, 12, 10, 12]}
    ),
    gleam@should:equal(gleam@list:try_map([4, 6, 5, 7, 3], Fun), {error, 7}).

drop_test() ->
    gleam@should:equal(gleam@list:drop([], 5), []),
    gleam@should:equal(gleam@list:drop([1, 2, 3, 4, 5, 6, 7, 8], 5), [6, 7, 8]).

take_test() ->
    gleam@should:equal(gleam@list:take([], 5), []),
    gleam@should:equal(
        gleam@list:take([1, 2, 3, 4, 5, 6, 7, 8], 5),
        [1, 2, 3, 4, 5]
    ).

new_test() ->
    gleam@should:equal(gleam@list:new(), []).

append_test() ->
    gleam@should:equal(gleam@list:append([1], [2, 3]), [1, 2, 3]).

flatten_test() ->
    gleam@should:equal(gleam@list:flatten([]), []),
    gleam@should:equal(gleam@list:flatten([[]]), []),
    gleam@should:equal(gleam@list:flatten([[], [], []]), []),
    gleam@should:equal(gleam@list:flatten([[1, 2], [], [3, 4]]), [1, 2, 3, 4]).

fold_test() ->
    gleam@should:equal(
        gleam@list:fold([1, 2, 3], [], fun(X, Acc) -> [X | Acc] end),
        [3, 2, 1]
    ).

fold_right_test() ->
    gleam@should:equal(
        gleam@list:fold_right([1, 2, 3], [], fun(X, Acc) -> [X | Acc] end),
        [1, 2, 3]
    ).

find_map_test() ->
    F = fun(X) -> case X of
            2 ->
                {ok, 4};

            _ ->
                {error, nil}
        end end,
    gleam@should:equal(gleam@list:find_map([1, 2, 3], F), {ok, 4}),
    gleam@should:equal(gleam@list:find_map([1, 3, 2], F), {ok, 4}),
    gleam@should:equal(gleam@list:find_map([1, 3], F), {error, nil}).

find_test() ->
    Is_two = fun(X) -> X =:= 2 end,
    gleam@should:equal(gleam@list:find([1, 2, 3], Is_two), {ok, 2}),
    gleam@should:equal(gleam@list:find([1, 3, 2], Is_two), {ok, 2}),
    gleam@should:equal(gleam@list:find([1, 3], Is_two), {error, nil}).

all_test() ->
    gleam@should:equal(
        gleam@list:all([1, 2, 3, 4, 5], fun(X) -> X > 0 end),
        true
    ),
    gleam@should:equal(
        gleam@list:all([1, 2, 3, 4, 5], fun(X@1) -> X@1 < 0 end),
        false
    ),
    gleam@should:equal(gleam@list:all([], fun(_) -> false end), true).

any_test() ->
    gleam@should:equal(
        gleam@list:any([1, 2, 3, 4, 5], fun(X) -> X =:= 2 end),
        true
    ),
    gleam@should:equal(
        gleam@list:any([1, 2, 3, 4, 5], fun(X@1) -> X@1 < 0 end),
        false
    ),
    gleam@should:equal(gleam@list:any([], fun(_) -> false end), false).

zip_test() ->
    gleam@should:equal(gleam@list:zip([], [1, 2, 3]), []),
    gleam@should:equal(gleam@list:zip([1, 2], []), []),
    gleam@should:equal(
        gleam@list:zip([1, 2, 3], [4, 5, 6]),
        [{1, 4}, {2, 5}, {3, 6}]
    ),
    gleam@should:equal(gleam@list:zip([5, 6], [1, 2, 3]), [{5, 1}, {6, 2}]),
    gleam@should:equal(gleam@list:zip([5, 6, 7], [1, 2]), [{5, 1}, {6, 2}]).

strict_zip_test() ->
    gleam@should:equal(
        gleam@list:strict_zip([], [1, 2, 3]),
        {error, length_mismatch}
    ),
    gleam@should:equal(
        gleam@list:strict_zip([1, 2], []),
        {error, length_mismatch}
    ),
    gleam@should:equal(
        gleam@list:strict_zip([1, 2, 3], [4, 5, 6]),
        {ok, [{1, 4}, {2, 5}, {3, 6}]}
    ),
    gleam@should:equal(
        gleam@list:strict_zip([5, 6], [1, 2, 3]),
        {error, length_mismatch}
    ),
    gleam@should:equal(
        gleam@list:strict_zip([5, 6, 7], [1, 2]),
        {error, length_mismatch}
    ).

unzip_test() ->
    gleam@should:equal(gleam@list:unzip([{1, 2}, {3, 4}]), {[1, 3], [2, 4]}),
    gleam@should:equal(gleam@list:unzip([]), {[], []}).

intersperse_test() ->
    gleam@should:equal(gleam@list:intersperse([1, 2, 3], 4), [1, 4, 2, 4, 3]),
    gleam@should:equal(gleam@list:intersperse([], 2), []).

at_test() ->
    gleam@should:equal(gleam@list:at([1, 2, 3], 2), {ok, 3}),
    gleam@should:equal(gleam@list:at([1, 2, 3], 5), {error, nil}),
    gleam@should:equal(gleam@list:at([], 0), {error, nil}),
    gleam@should:equal(gleam@list:at([1, 2, 3, 4, 5, 6], -1), {error, nil}).

unique_test() ->
    gleam@should:equal(
        gleam@list:unique([1, 1, 2, 3, 4, 4, 4, 5, 6]),
        [1, 2, 3, 4, 5, 6]
    ),
    gleam@should:equal(
        gleam@list:unique([7, 1, 45, 6, 2, 47, 2, 7, 5]),
        [7, 1, 45, 6, 2, 47, 5]
    ),
    gleam@should:equal(gleam@list:unique([3, 4, 5]), [3, 4, 5]),
    gleam@should:equal(gleam@list:unique([]), []).

sort_test() ->
    gleam@should:equal(
        gleam@list:sort([4, 3, 6, 5, 4], fun gleam@int:compare/2),
        [3, 4, 4, 5, 6]
    ),
    gleam@should:equal(
        gleam@list:sort([4, 3, 6, 5, 4, 1], fun gleam@int:compare/2),
        [1, 3, 4, 4, 5, 6]
    ),
    gleam@should:equal(
        gleam@list:sort([4.1, 3.1, 6.1, 5.1, 4.1], fun gleam@float:compare/2),
        [3.1, 4.1, 4.1, 5.1, 6.1]
    ),
    gleam@should:equal(gleam@list:sort([], fun gleam@int:compare/2), []).

index_map_test() ->
    gleam@should:equal(
        gleam@list:index_map([3, 4, 5], fun(I, X) -> {I, X} end),
        [{0, 3}, {1, 4}, {2, 5}]
    ),
    F = fun(I@1, X@1) -> gleam@string:append(X@1, gleam@int:to_string(I@1)) end,
    gleam@should:equal(
        gleam@list:index_map([<<"a"/utf8>>, <<"b"/utf8>>, <<"c"/utf8>>], F),
        [<<"a0"/utf8>>, <<"b1"/utf8>>, <<"c2"/utf8>>]
    ).

range_test() ->
    gleam@should:equal(gleam@list:range(0, 0), []),
    gleam@should:equal(gleam@list:range(1, 1), []),
    gleam@should:equal(gleam@list:range(-1, -1), []),
    gleam@should:equal(gleam@list:range(0, 1), [0]),
    gleam@should:equal(gleam@list:range(0, 5), [0, 1, 2, 3, 4]),
    gleam@should:equal(gleam@list:range(1, -5), [1, 0, -1, -2, -3, -4]).

repeat_test() ->
    gleam@should:equal(gleam@list:repeat(1, -10), []),
    gleam@should:equal(gleam@list:repeat(1, 0), []),
    gleam@should:equal(gleam@list:repeat(2, 3), [2, 2, 2]),
    gleam@should:equal(
        gleam@list:repeat(<<"x"/utf8>>, 5),
        [<<"x"/utf8>>, <<"x"/utf8>>, <<"x"/utf8>>, <<"x"/utf8>>, <<"x"/utf8>>]
    ).

split_test() ->
    gleam@should:equal(gleam@list:split([], 0), {[], []}),
    gleam@should:equal(
        gleam@list:split([0, 1, 2, 3, 4], 0),
        {[], [0, 1, 2, 3, 4]}
    ),
    gleam@should:equal(
        gleam@list:split([0, 1, 2, 3, 4], -2),
        {[], [0, 1, 2, 3, 4]}
    ),
    gleam@should:equal(
        gleam@list:split([0, 1, 2, 3, 4], 1),
        {[0], [1, 2, 3, 4]}
    ),
    gleam@should:equal(
        gleam@list:split([0, 1, 2, 3, 4], 3),
        {[0, 1, 2], [3, 4]}
    ),
    gleam@should:equal(
        gleam@list:split([0, 1, 2, 3, 4], 9),
        {[0, 1, 2, 3, 4], []}
    ).

split_while_test() ->
    gleam@should:equal(
        gleam@list:split_while([], fun(X) -> X =< 5 end),
        {[], []}
    ),
    gleam@should:equal(
        gleam@list:split_while([1, 2, 3, 4, 5], fun(X@1) -> X@1 =< 5 end),
        {[1, 2, 3, 4, 5], []}
    ),
    gleam@should:equal(
        gleam@list:split_while([1, 2, 3, 4, 5], fun(X@2) -> X@2 =:= 2 end),
        {[], [1, 2, 3, 4, 5]}
    ),
    gleam@should:equal(
        gleam@list:split_while([1, 2, 3, 4, 5], fun(X@3) -> X@3 =< 3 end),
        {[1, 2, 3], [4, 5]}
    ),
    gleam@should:equal(
        gleam@list:split_while([1, 2, 3, 4, 5], fun(X@4) -> X@4 =< -3 end),
        {[], [1, 2, 3, 4, 5]}
    ).

key_find_test() ->
    Proplist = [{0, <<"1"/utf8>>}, {1, <<"2"/utf8>>}],
    gleam@should:equal(gleam@list:key_find(Proplist, 0), {ok, <<"1"/utf8>>}),
    gleam@should:equal(gleam@list:key_find(Proplist, 1), {ok, <<"2"/utf8>>}),
    gleam@should:equal(gleam@list:key_find(Proplist, 2), {error, nil}).

pop_test() ->
    gleam@should:equal(
        gleam@list:pop([1, 2, 3], fun(X) -> X > 2 end),
        {ok, {3, [1, 2]}}
    ),
    gleam@should:equal(
        gleam@list:pop([1, 2, 3], fun(X@1) -> X@1 > 4 end),
        {error, nil}
    ),
    gleam@should:equal(gleam@list:pop([], fun(_) -> true end), {error, nil}).

pop_map_test() ->
    gleam@should:equal(
        gleam@list:pop_map(
            [<<"foo"/utf8>>, <<"2"/utf8>>, <<"3"/utf8>>],
            fun gleam@int:parse/1
        ),
        {ok, {2, [<<"foo"/utf8>>, <<"3"/utf8>>]}}
    ),
    gleam@should:equal(
        gleam@list:pop_map(
            [<<"foo"/utf8>>, <<"bar"/utf8>>],
            fun gleam@int:parse/1
        ),
        {error, nil}
    ),
    gleam@should:equal(
        gleam@list:pop_map([], fun gleam@int:parse/1),
        {error, nil}
    ).

key_pop_test() ->
    gleam@should:equal(
        gleam@list:key_pop([{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}], <<"a"/utf8>>),
        {ok, {0, [{<<"b"/utf8>>, 1}]}}
    ),
    gleam@should:equal(
        gleam@list:key_pop([{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}], <<"b"/utf8>>),
        {ok, {1, [{<<"a"/utf8>>, 0}]}}
    ),
    gleam@should:equal(
        gleam@list:key_pop([{<<"a"/utf8>>, 0}, {<<"b"/utf8>>, 1}], <<"c"/utf8>>),
        {error, nil}
    ).

key_set_test() ->
    gleam@should:equal(
        gleam@list:key_set([{5, 0}, {4, 1}], 4, 100),
        [{5, 0}, {4, 100}]
    ),
    gleam@should:equal(
        gleam@list:key_set([{5, 0}, {4, 1}], 1, 100),
        [{5, 0}, {4, 1}, {1, 100}]
    ).

partition_test() ->
    gleam@should:equal(
        gleam@list:partition([1, 2, 3, 4, 5, 6, 7], fun gleam@int:is_odd/1),
        {[1, 3, 5, 7], [2, 4, 6]}
    ).

permutations_test() ->
    gleam@should:equal(gleam@list:permutations([1, 2]), [[1, 2], [2, 1]]),
    Expected = [[1, 2, 3],
                [1, 3, 2],
                [2, 1, 3],
                [2, 3, 1],
                [3, 1, 2],
                [3, 2, 1]],
    gleam@should:equal(gleam@list:permutations([1, 2, 3]), Expected),
    gleam@should:equal(
        gleam@list:permutations([<<"a"/utf8>>, <<"b"/utf8>>]),
        [[<<"a"/utf8>>, <<"b"/utf8>>], [<<"b"/utf8>>, <<"a"/utf8>>]]
    ).
