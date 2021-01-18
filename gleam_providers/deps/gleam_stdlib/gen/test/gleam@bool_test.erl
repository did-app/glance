-module(gleam@bool_test).
-compile(no_auto_import).

-export([negate_test/0, nor_test/0, nand_test/0, exclusive_or_test/0, exclusive_nor_test/0, compare_test/0, max_test/0, min_test/0, to_int_test/0]).

negate_test() ->
    gleam@should:be_false(gleam@bool:negate(true)),
    gleam@should:be_true(gleam@bool:negate(false)).

nor_test() ->
    gleam@should:be_true(gleam@bool:nor(false, false)),
    gleam@should:be_false(gleam@bool:nor(false, true)),
    gleam@should:be_false(gleam@bool:nor(true, false)),
    gleam@should:be_false(gleam@bool:nor(true, true)).

nand_test() ->
    gleam@should:be_true(gleam@bool:nand(false, false)),
    gleam@should:be_true(gleam@bool:nand(false, true)),
    gleam@should:be_true(gleam@bool:nand(true, false)),
    gleam@should:be_false(gleam@bool:nand(true, true)).

exclusive_or_test() ->
    gleam@should:be_false(gleam@bool:exclusive_or(true, true)),
    gleam@should:be_false(gleam@bool:exclusive_or(false, false)),
    gleam@should:be_true(gleam@bool:exclusive_or(true, false)),
    gleam@should:be_true(gleam@bool:exclusive_or(false, true)).

exclusive_nor_test() ->
    gleam@should:be_true(gleam@bool:exclusive_nor(false, false)),
    gleam@should:be_false(gleam@bool:exclusive_nor(false, true)),
    gleam@should:be_false(gleam@bool:exclusive_nor(true, false)),
    gleam@should:be_true(gleam@bool:exclusive_nor(true, true)).

compare_test() ->
    gleam@should:equal(gleam@bool:compare(true, true), eq),
    gleam@should:equal(gleam@bool:compare(true, false), gt),
    gleam@should:equal(gleam@bool:compare(false, false), eq),
    gleam@should:equal(gleam@bool:compare(false, true), lt).

max_test() ->
    gleam@should:equal(gleam@bool:max(true, true), true),
    gleam@should:equal(gleam@bool:max(true, false), true),
    gleam@should:equal(gleam@bool:max(false, false), false),
    gleam@should:equal(gleam@bool:max(false, true), true).

min_test() ->
    gleam@should:equal(gleam@bool:min(true, true), true),
    gleam@should:equal(gleam@bool:min(true, false), false),
    gleam@should:equal(gleam@bool:min(false, false), false),
    gleam@should:equal(gleam@bool:min(false, true), false).

to_int_test() ->
    gleam@should:equal(gleam@bool:to_int(true), 1),
    gleam@should:equal(gleam@bool:to_int(false), 0).
