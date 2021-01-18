-module(gleam@float_test).
-compile(no_auto_import).

-export([parse_test/0, to_string_test/0, compare_test/0, ceiling_test/0, floor_test/0, round_test/0, truncate_test/0, min_test/0, max_test/0, absolute_value_test/0, power_test/0, square_root_test/0, negate_test/0, sum_test/0, product_test/0]).

parse_test() ->
    gleam@should:equal(gleam@float:parse(<<"1.23"/utf8>>), {ok, 1.23}),
    gleam@should:equal(gleam@float:parse(<<"5.0"/utf8>>), {ok, 5.0}),
    gleam@should:equal(
        gleam@float:parse(<<"0.123456789"/utf8>>),
        {ok, 0.123456789}
    ),
    gleam@should:equal(gleam@float:parse(<<""/utf8>>), {error, nil}),
    gleam@should:equal(gleam@float:parse(<<"what"/utf8>>), {error, nil}),
    gleam@should:equal(gleam@float:parse(<<"1"/utf8>>), {error, nil}).

to_string_test() ->
    gleam@should:equal(gleam@float:to_string(123.0), <<"123.0"/utf8>>),
    gleam@should:equal(gleam@float:to_string(-8.1), <<"-8.1"/utf8>>).

compare_test() ->
    gleam@should:equal(gleam@float:compare(0.0, 0.0), eq),
    gleam@should:equal(gleam@float:compare(0.1, 0.1), eq),
    gleam@should:equal(gleam@float:compare(0.0, 0.1), lt),
    gleam@should:equal(gleam@float:compare(-2.0, -1.9), lt),
    gleam@should:equal(gleam@float:compare(2.0, 1.9), gt),
    gleam@should:equal(gleam@float:compare(-1.9, -2.0), gt).

ceiling_test() ->
    gleam@should:equal(gleam@float:ceiling(8.1), 9.0),
    gleam@should:equal(gleam@float:ceiling(-8.1), -8.0),
    gleam@should:equal(gleam@float:ceiling(-8.0), -8.0).

floor_test() ->
    gleam@should:equal(gleam@float:floor(8.1), 8.0),
    gleam@should:equal(gleam@float:floor(-8.1), -9.0),
    gleam@should:equal(gleam@float:floor(-8.0), -8.0).

round_test() ->
    gleam@should:equal(gleam@float:round(8.1), 8),
    gleam@should:equal(gleam@float:round(8.4), 8),
    gleam@should:equal(gleam@float:round(8.499), 8),
    gleam@should:equal(gleam@float:round(8.5), 9),
    gleam@should:equal(gleam@float:round(-8.1), -8),
    gleam@should:equal(gleam@float:round(-7.5), -8).

truncate_test() ->
    gleam@should:equal(gleam@float:truncate(8.1), 8),
    gleam@should:equal(gleam@float:truncate(8.4), 8),
    gleam@should:equal(gleam@float:truncate(8.499), 8),
    gleam@should:equal(gleam@float:truncate(8.5), 8),
    gleam@should:equal(gleam@float:truncate(-8.1), -8),
    gleam@should:equal(gleam@float:truncate(-7.5), -7).

min_test() ->
    gleam@should:equal(gleam@float:min(0.0, 0.0), 0.0),
    gleam@should:equal(gleam@float:min(0.3, 1.5), 0.3),
    gleam@should:equal(gleam@float:min(1.0, 0.0), 0.0),
    gleam@should:equal(gleam@float:min(-1.7, 2.5), -1.7),
    gleam@should:equal(gleam@float:min(-2.2, -2.2), -2.2),
    gleam@should:equal(gleam@float:min(-1.0, -1.0), -1.0),
    gleam@should:equal(gleam@float:min(-1.1, -1.0), -1.1).

max_test() ->
    gleam@should:equal(gleam@float:max(0.0, 0.0), 0.0),
    gleam@should:equal(gleam@float:max(0.3, 1.5), 1.5),
    gleam@should:equal(gleam@float:max(1.0, 0.0), 1.0),
    gleam@should:equal(gleam@float:max(-1.7, 2.5), 2.5),
    gleam@should:equal(gleam@float:max(-2.2, -2.2), -2.2),
    gleam@should:equal(gleam@float:max(-1.0, -1.0), -1.0),
    gleam@should:equal(gleam@float:max(-1.1, -1.0), -1.0).

absolute_value_test() ->
    gleam@should:equal(gleam@float:absolute_value(-1.0), 1.0),
    gleam@should:equal(gleam@float:absolute_value(-20.6), 20.6),
    gleam@should:equal(gleam@float:absolute_value(0.0), 0.0),
    gleam@should:equal(gleam@float:absolute_value(1.0), 1.0),
    gleam@should:equal(gleam@float:absolute_value(25.2), 25.2).

power_test() ->
    gleam@should:equal(gleam@float:power(2.0, 2.0), 4.0),
    gleam@should:equal(gleam@float:power(-5.0, 3.0), -125.0),
    gleam@should:equal(gleam@float:power(10.5, 0.0), 1.0),
    gleam@should:equal(gleam@float:power(16.0, 0.5), 4.0),
    gleam@should:equal(gleam@float:power(2.0, -1.0), 0.5).

square_root_test() ->
    gleam@should:equal(gleam@float:square_root(4.0), {ok, 2.0}),
    gleam@should:equal(gleam@float:square_root(16.0), {ok, 4.0}),
    gleam@should:equal(gleam@float:square_root(0.0), {ok, 0.0}),
    gleam@should:equal(gleam@float:square_root(-4.0), {error, nil}).

negate_test() ->
    gleam@should:equal(gleam@float:negate(-1.0), 1.0),
    gleam@should:equal(gleam@float:negate(2.0), -2.0),
    gleam@should:equal(gleam@float:negate(0.0), 0.0).

sum_test() ->
    gleam@should:equal(gleam@float:sum([]), 0.0),
    gleam@should:equal(gleam@float:sum([1.0, 2.2, 3.3]), 6.5).

product_test() ->
    gleam@should:equal(gleam@float:product([]), 0.0),
    gleam@should:equal(gleam@float:product([4.0]), 4.0),
    gleam@should:equal(gleam@float:product([2.5, 3.2, 4.2]), 33.6).
