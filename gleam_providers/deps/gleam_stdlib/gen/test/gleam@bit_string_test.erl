-module(gleam@bit_string_test).
-compile(no_auto_import).

-export([length_test/0, append_test/0, part_test/0, u32_test/0, to_string_test/0]).

length_test() ->
    gleam@should:equal(
        gleam@bit_string:byte_size(
            gleam@bit_string:from_string(<<"hello"/utf8>>)
        ),
        5
    ),
    gleam@should:equal(
        gleam@bit_string:byte_size(gleam@bit_string:from_string(<<""/utf8>>)),
        0
    ).

append_test() ->
    gleam@should:equal(
        gleam@bit_string:append(
            gleam@bit_string:from_string(<<"Test"/utf8>>),
            gleam@bit_string:from_string(<<" Me"/utf8>>)
        ),
        gleam@bit_string:from_string(<<"Test Me"/utf8>>)
    ),
    {ok, Zero_32bit} = gleam@bit_string:int_to_u32(0),
    gleam@should:equal(
        gleam@bit_string:append(
            Zero_32bit,
            gleam@bit_string:from_string(<<""/utf8>>)
        ),
        Zero_32bit
    ).

part_test() ->
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            0,
            5
        ),
        {ok, gleam@bit_string:from_string(<<"hello"/utf8>>)}
    ),
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            0,
            0
        ),
        {ok, gleam@bit_string:from_string(<<""/utf8>>)}
    ),
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            2,
            2
        ),
        {ok, gleam@bit_string:from_string(<<"ll"/utf8>>)}
    ),
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            5,
            -2
        ),
        {ok, gleam@bit_string:from_string(<<"lo"/utf8>>)}
    ),
    gleam@should:equal(
        gleam@bit_string:part(gleam@bit_string:from_string(<<""/utf8>>), 0, 0),
        {ok, gleam@bit_string:from_string(<<""/utf8>>)}
    ),
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            6,
            0
        ),
        {error, nil}
    ),
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            -1,
            1
        ),
        {error, nil}
    ),
    gleam@should:equal(
        gleam@bit_string:part(
            gleam@bit_string:from_string(<<"hello"/utf8>>),
            1,
            6
        ),
        {error, nil}
    ).

u32_test() ->
    {ok, Bin} = gleam@bit_string:int_to_u32(0),
    gleam@should:equal(4, gleam@bit_string:byte_size(Bin)),
    gleam@should:equal({ok, 0}, gleam@bit_string:int_from_u32(Bin)),
    {ok, Bin@1} = gleam@bit_string:int_to_u32(4294967295),
    gleam@should:equal(4, gleam@bit_string:byte_size(Bin@1)),
    gleam@should:equal({ok, 4294967295}, gleam@bit_string:int_from_u32(Bin@1)),
    gleam@should:equal(
        {error, nil},
        gleam@bit_string:int_from_u32(gleam@bit_string:from_string(<<""/utf8>>))
    ),
    gleam@should:equal(
        {error, nil},
        gleam@bit_string:int_from_u32(
            gleam@bit_string:from_string(<<"12345"/utf8>>)
        )
    ).

to_string_test() ->
    gleam@should:equal(gleam@bit_string:to_string(<<>>), {ok, <<""/utf8>>}),
    gleam@should:equal(
        gleam@bit_string:to_string(<<""/utf8>>),
        {ok, <<""/utf8>>}
    ),
    gleam@should:equal(
        gleam@bit_string:to_string(<<"Hello"/utf8>>),
        {ok, <<"Hello"/utf8>>}
    ),
    gleam@should:equal(
        gleam@bit_string:to_string(<<"ø"/utf8>>),
        {ok, <<"ø"/utf8>>}
    ),
    gleam@should:equal(gleam@bit_string:to_string(<<65535:16>>), {error, nil}).
