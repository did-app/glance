-module(gleam@base_test).
-compile(no_auto_import).

-export([encode64_test/0, decode64_test/0, url_encode64_test/0, url_decode64_test/0]).

encode64_test() ->
    gleam@should:equal(
        gleam@base:encode64(erlang:list_to_binary([255, 127, 254, 252]), true),
        <<"/3/+/A=="/utf8>>
    ),
    gleam@should:equal(
        gleam@base:encode64(erlang:list_to_binary([255, 127, 254, 252]), false),
        <<"/3/+/A"/utf8>>
    ),
    gleam@should:equal(
        gleam@base:encode64(erlang:list_to_binary([0, 0, 0]), true),
        <<"AAAA"/utf8>>
    ),
    gleam@should:equal(
        gleam@base:encode64(erlang:list_to_binary([]), true),
        <<""/utf8>>
    ).

decode64_test() ->
    gleam@should:equal(
        gleam@base:decode64(<<"/3/+/A=="/utf8>>),
        {ok, erlang:list_to_binary([255, 127, 254, 252])}
    ),
    gleam@should:equal(
        gleam@base:decode64(<<"/3/+/A"/utf8>>),
        {ok, erlang:list_to_binary([255, 127, 254, 252])}
    ),
    gleam@should:equal(
        gleam@base:decode64(<<"AAAA"/utf8>>),
        {ok, erlang:list_to_binary([0, 0, 0])}
    ),
    gleam@should:equal(
        gleam@base:decode64(<<""/utf8>>),
        {ok, erlang:list_to_binary([])}
    ),
    gleam@should:equal(gleam@base:decode64(<<")!"/utf8>>), {error, nil}).

url_encode64_test() ->
    gleam@should:equal(
        gleam@base:url_encode64(
            erlang:list_to_binary([255, 127, 254, 252]),
            true
        ),
        <<"_3_-_A=="/utf8>>
    ),
    gleam@should:equal(
        gleam@base:url_encode64(
            erlang:list_to_binary([255, 127, 254, 252]),
            false
        ),
        <<"_3_-_A"/utf8>>
    ),
    gleam@should:equal(
        gleam@base:url_encode64(erlang:list_to_binary([0, 0, 0]), true),
        <<"AAAA"/utf8>>
    ),
    gleam@should:equal(
        gleam@base:url_encode64(erlang:list_to_binary([]), true),
        <<""/utf8>>
    ).

url_decode64_test() ->
    gleam@should:equal(
        gleam@base:url_decode64(<<"_3_-_A=="/utf8>>),
        {ok, erlang:list_to_binary([255, 127, 254, 252])}
    ),
    gleam@should:equal(
        gleam@base:url_decode64(<<"_3_-_A"/utf8>>),
        {ok, erlang:list_to_binary([255, 127, 254, 252])}
    ),
    gleam@should:equal(
        gleam@base:url_decode64(<<"AAAA"/utf8>>),
        {ok, erlang:list_to_binary([0, 0, 0])}
    ),
    gleam@should:equal(
        gleam@base:url_decode64(<<""/utf8>>),
        {ok, erlang:list_to_binary([])}
    ),
    gleam@should:equal(gleam@base:url_decode64(<<")!"/utf8>>), {error, nil}).
