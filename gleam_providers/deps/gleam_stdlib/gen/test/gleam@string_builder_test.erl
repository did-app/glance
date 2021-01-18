-module(gleam@string_builder_test).
-compile(no_auto_import).

-export([string_builder_test/0, lowercase_test/0, uppercase_test/0, split_test/0, is_equal_test/0, is_empty_test/0]).

string_builder_test() ->
    Data = gleam@string_builder:prepend(
        gleam@string_builder:append(
            gleam@string_builder:append(
                gleam@string_builder:from_string(<<"ello"/utf8>>),
                <<","/utf8>>
            ),
            <<" world!"/utf8>>
        ),
        <<"H"/utf8>>
    ),
    gleam@should:equal(
        gleam@string_builder:to_string(Data),
        <<"Hello, world!"/utf8>>
    ),
    gleam@should:equal(gleam@string_builder:byte_size(Data), 13),
    Data@1 = gleam@string_builder:prepend_builder(
        gleam@string_builder:append_builder(
            gleam@string_builder:append_builder(
                gleam@string_builder:from_string(<<"ello"/utf8>>),
                gleam@string_builder:from_string(<<","/utf8>>)
            ),
            gleam@string_builder:concat(
                [gleam@string_builder:from_string(<<" wo"/utf8>>),
                 gleam@string_builder:from_string(<<"rld!"/utf8>>)]
            )
        ),
        gleam@string_builder:from_string(<<"H"/utf8>>)
    ),
    gleam@should:equal(
        gleam@string_builder:to_string(Data@1),
        <<"Hello, world!"/utf8>>
    ),
    gleam@should:equal(gleam@string_builder:byte_size(Data@1), 13).

lowercase_test() ->
    gleam@should:equal(
        gleam@string_builder:to_string(
            gleam@string_builder:lowercase(
                gleam@string_builder:from_strings(
                    [<<"Gleam"/utf8>>, <<"Gleam"/utf8>>]
                )
            )
        ),
        <<"gleamgleam"/utf8>>
    ).

uppercase_test() ->
    gleam@should:equal(
        gleam@string_builder:to_string(
            gleam@string_builder:uppercase(
                gleam@string_builder:from_strings(
                    [<<"Gleam"/utf8>>, <<"Gleam"/utf8>>]
                )
            )
        ),
        <<"GLEAMGLEAM"/utf8>>
    ).

split_test() ->
    gleam@should:equal(
        gleam@string_builder:split(
            gleam@string_builder:from_string(<<"Gleam,Erlang,Elixir"/utf8>>),
            <<","/utf8>>
        ),
        [gleam@string_builder:from_string(<<"Gleam"/utf8>>),
         gleam@string_builder:from_string(<<"Erlang"/utf8>>),
         gleam@string_builder:from_string(<<"Elixir"/utf8>>)]
    ),
    gleam@should:equal(
        gleam@string_builder:split(
            gleam@string_builder:from_strings(
                [<<"Gleam, Erl"/utf8>>, <<"ang,Elixir"/utf8>>]
            ),
            <<", "/utf8>>
        ),
        [gleam@string_builder:from_string(<<"Gleam"/utf8>>),
         gleam@string_builder:from_strings(
             [<<"Erl"/utf8>>, <<"ang,Elixir"/utf8>>]
         )]
    ).

is_equal_test() ->
    gleam@should:be_true(
        gleam@string_builder:is_equal(
            gleam@string_builder:from_string(<<"12"/utf8>>),
            gleam@string_builder:from_strings([<<"1"/utf8>>, <<"2"/utf8>>])
        )
    ),
    gleam@should:be_true(
        gleam@string_builder:is_equal(
            gleam@string_builder:from_string(<<"12"/utf8>>),
            gleam@string_builder:from_string(<<"12"/utf8>>)
        )
    ),
    gleam@should:be_false(
        gleam@string_builder:is_equal(
            gleam@string_builder:from_string(<<"12"/utf8>>),
            gleam@string_builder:from_string(<<"2"/utf8>>)
        )
    ).

is_empty_test() ->
    gleam@should:be_true(
        gleam@string_builder:is_empty(
            gleam@string_builder:from_string(<<""/utf8>>)
        )
    ),
    gleam@should:be_false(
        gleam@string_builder:is_empty(
            gleam@string_builder:from_string(<<"12"/utf8>>)
        )
    ),
    gleam@should:be_true(
        gleam@string_builder:is_empty(gleam@string_builder:from_strings([]))
    ),
    gleam@should:be_true(
        gleam@string_builder:is_empty(
            gleam@string_builder:from_strings([<<""/utf8>>, <<""/utf8>>])
        )
    ).
