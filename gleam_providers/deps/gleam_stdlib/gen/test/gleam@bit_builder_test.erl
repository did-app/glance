-module(gleam@bit_builder_test).
-compile(no_auto_import).

-export([builder_test/0, builder_with_strings_test/0, builder_with_builders_test/0, concat_test/0, from_bit_string_test/0, from_string_test/0]).

builder_test() ->
    Data = gleam@bit_builder:prepend(
        gleam@bit_builder:append(
            gleam@bit_builder:append(
                gleam@bit_builder:from_bit_string(<<1>>),
                <<2>>
            ),
            <<3>>
        ),
        <<0>>
    ),
    gleam@should:equal(gleam@bit_builder:to_bit_string(Data), <<0, 1, 2, 3>>),
    gleam@should:equal(gleam@bit_builder:byte_size(Data), 4).

builder_with_strings_test() ->
    Data = gleam@bit_builder:prepend_string(
        gleam@bit_builder:append_string(
            gleam@bit_builder:append_string(
                gleam@bit_builder:from_bit_string(<<1>>),
                <<"2"/utf8>>
            ),
            <<"3"/utf8>>
        ),
        <<"0"/utf8>>
    ),
    gleam@should:equal(
        gleam@bit_builder:to_bit_string(Data),
        <<"0"/utf8, 1, "2"/utf8, "3"/utf8>>
    ),
    gleam@should:equal(gleam@bit_builder:byte_size(Data), 4).

builder_with_builders_test() ->
    Data = gleam@bit_builder:prepend_builder(
        gleam@bit_builder:append_builder(
            gleam@bit_builder:append_builder(
                gleam@bit_builder:from_bit_string(<<1>>),
                gleam@bit_builder:from_bit_string(<<2>>)
            ),
            gleam@bit_builder:from_bit_string(<<3>>)
        ),
        gleam@bit_builder:from_bit_string(<<0>>)
    ),
    gleam@should:equal(gleam@bit_builder:to_bit_string(Data), <<0, 1, 2, 3>>),
    gleam@should:equal(gleam@bit_builder:byte_size(Data), 4).

concat_test() ->
    gleam@should:equal(
        gleam@bit_builder:to_bit_string(
            gleam@bit_builder:concat(
                [gleam@bit_builder:from_bit_string(<<1, 2>>),
                 gleam@bit_builder:from_bit_string(<<3, 4>>),
                 gleam@bit_builder:from_bit_string(<<5, 6>>)]
            )
        ),
        <<1, 2, 3, 4, 5, 6>>
    ).

from_bit_string_test() ->
    gleam@should:equal(
        gleam@bit_builder:to_bit_string(gleam@bit_builder:from_bit_string(<<>>)),
        <<>>
    ).

from_string_test() ->
    gleam@should:equal(
        gleam@bit_builder:to_bit_string(
            gleam@bit_builder:from_string(<<""/utf8>>)
        ),
        <<>>
    ).
