-module(gleam@string_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, split_once_test/0, replace_test/0, append_test/0, compare_test/0, contains_test/0, concat_test/0, repeat_test/0, join_test/0, trim_test/0, trim_left_test/0, trim_right_test/0, starts_with_test/0, ends_with_test/0, slice_test/0, drop_left_test/0, drop_right_test/0, pad_left_test/0, pad_right_test/0, pop_grapheme_test/0, to_graphemes_test/0, utf_codepoint_test/0]).

length_test() ->
    gleam@should:equal(gleam@string:length(<<"ß↑e̊"/utf8>>), 3),
    gleam@should:equal(gleam@string:length(<<"Gleam"/utf8>>), 5),
    gleam@should:equal(gleam@string:length(<<""/utf8>>), 0).

lowercase_test() ->
    gleam@should:equal(
        gleam@string:lowercase(<<"Gleam"/utf8>>),
        <<"gleam"/utf8>>
    ).

uppercase_test() ->
    gleam@should:equal(
        gleam@string:uppercase(<<"Gleam"/utf8>>),
        <<"GLEAM"/utf8>>
    ).

reverse_test() ->
    gleam@should:equal(gleam@string:reverse(<<"Gleam"/utf8>>), <<"maelG"/utf8>>).

split_test() ->
    gleam@should:equal(
        gleam@string:split(<<"Gleam,Erlang,Elixir"/utf8>>, <<","/utf8>>),
        [<<"Gleam"/utf8>>, <<"Erlang"/utf8>>, <<"Elixir"/utf8>>]
    ),
    gleam@should:equal(
        gleam@string:split(<<"Gleam, Erlang,Elixir"/utf8>>, <<", "/utf8>>),
        [<<"Gleam"/utf8>>, <<"Erlang,Elixir"/utf8>>]
    ).

split_once_test() ->
    gleam@should:equal(
        gleam@string:split_once(<<"Gleam,Erlang,Elixir"/utf8>>, <<","/utf8>>),
        {ok, {<<"Gleam"/utf8>>, <<"Erlang,Elixir"/utf8>>}}
    ),
    gleam@should:equal(
        gleam@string:split_once(<<"Gleam"/utf8>>, <<","/utf8>>),
        {error, nil}
    ),
    gleam@should:equal(
        gleam@string:split_once(<<""/utf8>>, <<","/utf8>>),
        {error, nil}
    ).

replace_test() ->
    gleam@should:equal(
        gleam@string:replace(
            <<"Gleam,Erlang,Elixir"/utf8>>,
            <<","/utf8>>,
            <<"++"/utf8>>
        ),
        <<"Gleam++Erlang++Elixir"/utf8>>
    ).

append_test() ->
    gleam@should:equal(
        gleam@string:append(<<"Test"/utf8>>, <<" Me"/utf8>>),
        <<"Test Me"/utf8>>
    ).

compare_test() ->
    gleam@should:equal(gleam@string:compare(<<""/utf8>>, <<""/utf8>>), eq),
    gleam@should:equal(gleam@string:compare(<<"a"/utf8>>, <<""/utf8>>), gt),
    gleam@should:equal(gleam@string:compare(<<"a"/utf8>>, <<"A"/utf8>>), gt),
    gleam@should:equal(gleam@string:compare(<<"A"/utf8>>, <<"B"/utf8>>), lt),
    gleam@should:equal(gleam@string:compare(<<"t"/utf8>>, <<"ABC"/utf8>>), gt).

contains_test() ->
    gleam@should:equal(
        gleam@string:contains(<<"gleam"/utf8>>, <<"ea"/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:contains(<<"gleam"/utf8>>, <<"x"/utf8>>),
        false
    ),
    gleam@should:equal(
        gleam@string:contains(<<"bellwether"/utf8>>, <<"bell"/utf8>>),
        true
    ).

concat_test() ->
    gleam@should:equal(
        gleam@string:concat(
            [<<"Hello"/utf8>>, <<", "/utf8>>, <<"world!"/utf8>>]
        ),
        <<"Hello, world!"/utf8>>
    ).

repeat_test() ->
    gleam@should:equal(gleam@string:repeat(<<"hi"/utf8>>, 3), <<"hihihi"/utf8>>),
    gleam@should:equal(gleam@string:repeat(<<"hi"/utf8>>, 0), <<""/utf8>>),
    gleam@should:equal(gleam@string:repeat(<<"hi"/utf8>>, -1), <<""/utf8>>).

join_test() ->
    gleam@should:equal(
        gleam@string:join([<<"Hello"/utf8>>, <<"world!"/utf8>>], <<", "/utf8>>),
        <<"Hello, world!"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:join([<<"Hello"/utf8>>, <<"world!"/utf8>>], <<"-"/utf8>>),
        <<"Hello-world!"/utf8>>
    ).

trim_test() ->
    gleam@should:equal(
        gleam@string:trim(<<"  hats  \n"/utf8>>),
        <<"hats"/utf8>>
    ).

trim_left_test() ->
    gleam@should:equal(
        gleam@string:trim_left(<<"  hats  \n"/utf8>>),
        <<"hats  \n"/utf8>>
    ).

trim_right_test() ->
    gleam@should:equal(
        gleam@string:trim_right(<<"  hats  \n"/utf8>>),
        <<"  hats"/utf8>>
    ).

starts_with_test() ->
    gleam@should:equal(
        gleam@string:starts_with(<<"theory"/utf8>>, <<""/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:starts_with(<<"theory"/utf8>>, <<"the"/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:starts_with(<<"theory"/utf8>>, <<"ory"/utf8>>),
        false
    ),
    gleam@should:equal(
        gleam@string:starts_with(<<"theory"/utf8>>, <<"theory2"/utf8>>),
        false
    ).

ends_with_test() ->
    gleam@should:equal(
        gleam@string:ends_with(<<"theory"/utf8>>, <<""/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:ends_with(<<"theory"/utf8>>, <<"ory"/utf8>>),
        true
    ),
    gleam@should:equal(
        gleam@string:ends_with(<<"theory"/utf8>>, <<"the"/utf8>>),
        false
    ),
    gleam@should:equal(
        gleam@string:ends_with(<<"theory"/utf8>>, <<"theory2"/utf8>>),
        false
    ).

slice_test() ->
    gleam@should:equal(
        gleam@string:slice(<<"gleam"/utf8>>, 1, 2),
        <<"le"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:slice(<<"gleam"/utf8>>, 1, 10),
        <<"leam"/utf8>>
    ),
    gleam@should:equal(gleam@string:slice(<<"gleam"/utf8>>, 10, 3), <<""/utf8>>),
    gleam@should:equal(
        gleam@string:slice(<<"gleam"/utf8>>, -2, 2),
        <<"am"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:slice(<<"gleam"/utf8>>, -12, 2),
        <<""/utf8>>
    ),
    gleam@should:equal(gleam@string:slice(<<"gleam"/utf8>>, 2, -3), <<""/utf8>>).

drop_left_test() ->
    gleam@should:equal(
        gleam@string:drop_left(<<"gleam"/utf8>>, 2),
        <<"eam"/utf8>>
    ),
    gleam@should:equal(gleam@string:drop_left(<<"gleam"/utf8>>, 6), <<""/utf8>>),
    gleam@should:equal(
        gleam@string:drop_left(<<"gleam"/utf8>>, -2),
        <<"gleam"/utf8>>
    ).

drop_right_test() ->
    gleam@should:equal(
        gleam@string:drop_right(<<"gleam"/utf8>>, 2),
        <<"gle"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:drop_right(<<"gleam"/utf8>>, 5),
        <<""/utf8>>
    ),
    gleam@should:equal(
        gleam@string:drop_right(<<"gleam"/utf8>>, -2),
        <<"gleam"/utf8>>
    ).

pad_left_test() ->
    gleam@should:equal(
        gleam@string:pad_left(<<"121"/utf8>>, 5, <<"."/utf8>>),
        <<"..121"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<"121"/utf8>>, 3, <<"."/utf8>>),
        <<"121"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<"121"/utf8>>, 2, <<"."/utf8>>),
        <<"121"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_left(<<"121"/utf8>>, 5, <<"XY"/utf8>>),
        <<"XYXY121"/utf8>>
    ).

pad_right_test() ->
    gleam@should:equal(
        gleam@string:pad_right(<<"121"/utf8>>, 5, <<"."/utf8>>),
        <<"121.."/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<"121"/utf8>>, 3, <<"."/utf8>>),
        <<"121"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<"121"/utf8>>, 2, <<"."/utf8>>),
        <<"121"/utf8>>
    ),
    gleam@should:equal(
        gleam@string:pad_right(<<"121"/utf8>>, 5, <<"XY"/utf8>>),
        <<"121XYXY"/utf8>>
    ).

pop_grapheme_test() ->
    gleam@should:equal(
        gleam@string:pop_grapheme(<<"gleam"/utf8>>),
        {ok, {<<"g"/utf8>>, <<"leam"/utf8>>}}
    ),
    gleam@should:equal(
        gleam@string:pop_grapheme(<<"g"/utf8>>),
        {ok, {<<"g"/utf8>>, <<""/utf8>>}}
    ),
    gleam@should:equal(gleam@string:pop_grapheme(<<""/utf8>>), {error, nil}).

to_graphemes_test() ->
    gleam@should:equal(
        gleam@string:to_graphemes(<<"abc"/utf8>>),
        [<<"a"/utf8>>, <<"b"/utf8>>, <<"c"/utf8>>]
    ),
    gleam@should:equal(gleam@string:to_graphemes(<<"a"/utf8>>), [<<"a"/utf8>>]),
    gleam@should:equal(gleam@string:to_graphemes(<<""/utf8>>), []).

utf_codepoint_test() ->
    gleam@should:be_error(gleam@string:utf_codepoint(1114444)),
    gleam@should:be_error(gleam@string:utf_codepoint(65534)),
    gleam@should:be_error(gleam@string:utf_codepoint(55296)),
    {ok, Snake} = gleam@string:utf_codepoint(128013),
    gleam@should:equal(<<Snake/utf8>>, <<"🐍"/utf8>>).