-module(gleam@option_test).
-compile(no_auto_import).

-export([is_some_test/0, is_none_test/0, to_result_test/0, from_result_test/0, unwrap_option_test/0, map_option_test/0, flatten_option_test/0, then_option_test/0, or_option_test/0]).

is_some_test() ->
    gleam@should:be_true(gleam@option:is_some({some, 1})),
    gleam@should:be_false(gleam@option:is_some(none)).

is_none_test() ->
    gleam@should:be_false(gleam@option:is_none({some, 1})),
    gleam@should:be_true(gleam@option:is_none(none)).

to_result_test() ->
    gleam@should:equal(
        gleam@option:to_result({some, 1}, <<"possible_error"/utf8>>),
        {ok, 1}
    ),
    gleam@should:equal(
        gleam@option:to_result(none, <<"possible_error"/utf8>>),
        {error, <<"possible_error"/utf8>>}
    ).

from_result_test() ->
    gleam@should:equal(gleam@option:from_result({ok, 1}), {some, 1}),
    gleam@should:equal(
        gleam@option:from_result({error, <<"some_error"/utf8>>}),
        none
    ).

unwrap_option_test() ->
    gleam@should:equal(gleam@option:unwrap({some, 1}, 0), 1),
    gleam@should:equal(gleam@option:unwrap(none, 0), 0).

map_option_test() ->
    gleam@should:equal(
        gleam@option:map({some, 1}, fun(X) -> X + 1 end),
        {some, 2}
    ),
    gleam@should:equal(
        gleam@option:map({some, 1}, fun(_) -> <<"2"/utf8>> end),
        {some, <<"2"/utf8>>}
    ),
    gleam@should:equal(gleam@option:map(none, fun(X@1) -> X@1 + 1 end), none).

flatten_option_test() ->
    gleam@should:equal(gleam@option:flatten({some, {some, 1}}), {some, 1}),
    gleam@should:equal(gleam@option:flatten({some, none}), none),
    gleam@should:equal(gleam@option:flatten(none), none).

then_option_test() ->
    gleam@should:equal(
        gleam@option:then({some, 1}, fun(X) -> {some, X + 1} end),
        {some, 2}
    ),
    gleam@should:equal(
        gleam@option:then({some, 1}, fun(_) -> {some, <<"2"/utf8>>} end),
        {some, <<"2"/utf8>>}
    ),
    gleam@should:equal(
        gleam@option:then(none, fun(X@1) -> {some, X@1 + 1} end),
        none
    ).

or_option_test() ->
    gleam@should:equal(gleam@option:'or'({some, 1}, {some, 2}), {some, 1}),
    gleam@should:equal(gleam@option:'or'({some, 1}, none), {some, 1}),
    gleam@should:equal(gleam@option:'or'(none, {some, 2}), {some, 2}),
    gleam@should:equal(gleam@option:'or'(none, none), none).
