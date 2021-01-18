-module(gleam@result_test).
-compile(no_auto_import).

-export([is_ok_test/0, is_error_test/0, map_test/0, map_error_test/0, flatten_test/0, then_test/0, unwrap_test/0, lazy_unwrap_test/0, nil_error_test/0, or_test/0, lazy_or_test/0, all_test/0]).

is_ok_test() ->
    gleam@should:be_true(gleam@result:is_ok({ok, 1})),
    gleam@should:be_false(gleam@result:is_ok({error, 1})).

is_error_test() ->
    gleam@should:be_false(gleam@result:is_error({ok, 1})),
    gleam@should:be_true(gleam@result:is_error({error, 1})).

map_test() ->
    gleam@should:equal(gleam@result:map({ok, 1}, fun(X) -> X + 1 end), {ok, 2}),
    gleam@should:equal(
        gleam@result:map({ok, 1}, fun(_) -> <<"2"/utf8>> end),
        {ok, <<"2"/utf8>>}
    ),
    gleam@should:equal(
        gleam@result:map({error, 1}, fun(X@1) -> X@1 + 1 end),
        {error, 1}
    ).

map_error_test() ->
    gleam@should:equal(
        gleam@result:map_error({ok, 1}, fun(X) -> X + 1 end),
        {ok, 1}
    ),
    gleam@should:equal(
        gleam@result:map_error(
            {error, 1},
            fun(X@1) -> {<<"ok"/utf8>>, X@1 + 1} end
        ),
        {error, {<<"ok"/utf8>>, 2}}
    ).

flatten_test() ->
    gleam@should:equal(gleam@result:flatten({ok, {ok, 1}}), {ok, 1}),
    gleam@should:equal(gleam@result:flatten({ok, {error, 1}}), {error, 1}),
    gleam@should:equal(gleam@result:flatten({error, 1}), {error, 1}),
    gleam@should:equal(
        gleam@result:flatten({error, {error, 1}}),
        {error, {error, 1}}
    ).

then_test() ->
    gleam@should:equal(
        gleam@result:then({error, 1}, fun(X) -> {ok, X + 1} end),
        {error, 1}
    ),
    gleam@should:equal(
        gleam@result:then({ok, 1}, fun(X@1) -> {ok, X@1 + 1} end),
        {ok, 2}
    ),
    gleam@should:equal(
        gleam@result:then({ok, 1}, fun(_) -> {ok, <<"type change"/utf8>>} end),
        {ok, <<"type change"/utf8>>}
    ),
    gleam@should:equal(
        gleam@result:then({ok, 1}, fun(_) -> {error, 1} end),
        {error, 1}
    ).

unwrap_test() ->
    gleam@should:equal(gleam@result:unwrap({ok, 1}, 50), 1),
    gleam@should:equal(gleam@result:unwrap({error, <<"nope"/utf8>>}, 50), 50).

lazy_unwrap_test() ->
    gleam@should:equal(gleam@result:lazy_unwrap({ok, 1}, fun() -> 50 end), 1),
    gleam@should:equal(
        gleam@result:lazy_unwrap({error, <<"nope"/utf8>>}, fun() -> 50 end),
        50
    ).

nil_error_test() ->
    gleam@should:equal(
        gleam@result:nil_error({error, <<"error_string"/utf8>>}),
        {error, nil}
    ),
    gleam@should:equal(gleam@result:nil_error({error, 123}), {error, nil}),
    gleam@should:equal(gleam@result:nil_error({ok, 1}), {ok, 1}).

or_test() ->
    gleam@should:equal(gleam@result:'or'({ok, 1}, {ok, 2}), {ok, 1}),
    gleam@should:equal(
        gleam@result:'or'({ok, 1}, {error, <<"Error 2"/utf8>>}),
        {ok, 1}
    ),
    gleam@should:equal(
        gleam@result:'or'({error, <<"Error 1"/utf8>>}, {ok, 2}),
        {ok, 2}
    ),
    gleam@should:equal(
        gleam@result:'or'(
            {error, <<"Error 1"/utf8>>},
            {error, <<"Error 2"/utf8>>}
        ),
        {error, <<"Error 2"/utf8>>}
    ).

lazy_or_test() ->
    gleam@should:equal(
        gleam@result:lazy_or({ok, 1}, fun() -> {ok, 2} end),
        {ok, 1}
    ),
    gleam@should:equal(
        gleam@result:lazy_or({ok, 1}, fun() -> {error, <<"Error 2"/utf8>>} end),
        {ok, 1}
    ),
    gleam@should:equal(
        gleam@result:lazy_or({error, <<"Error 1"/utf8>>}, fun() -> {ok, 2} end),
        {ok, 2}
    ),
    gleam@should:equal(
        gleam@result:lazy_or(
            {error, <<"Error 1"/utf8>>},
            fun() -> {error, <<"Error 2"/utf8>>} end
        ),
        {error, <<"Error 2"/utf8>>}
    ).

all_test() ->
    gleam@should:equal(
        gleam@result:all([{ok, 1}, {ok, 2}, {ok, 3}]),
        {ok, [1, 2, 3]}
    ),
    gleam@should:equal(
        gleam@result:all(
            [{ok, 1}, {error, <<"a"/utf8>>}, {error, <<"b"/utf8>>}, {ok, 3}]
        ),
        {error, <<"a"/utf8>>}
    ).
