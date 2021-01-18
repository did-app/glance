-module(gleam@os_test).
-compile(no_auto_import).

-export([env_test/0, system_time_test/0, erlang_timestamp_test/0]).

env_test() ->
    gleam@os:insert_env(<<"GLEAM_TEST"/utf8>>, <<"hello"/utf8>>),
    gleam@should:equal(
        gleam@map:get(gleam@os:get_env(), <<"GLEAM_TEST"/utf8>>),
        {ok, <<"hello"/utf8>>}
    ),
    gleam@os:delete_env(<<"GLEAM_TEST"/utf8>>),
    gleam@should:equal(
        gleam@map:get(gleam@os:get_env(), <<"GLEAM_TEST"/utf8>>),
        {error, nil}
    ).

system_time_test() ->
    June_12_2020 = 1591966971,
    gleam@should:equal(gleam@os:system_time(second) > June_12_2020, true),
    gleam@should:equal(
        gleam@os:system_time(second)
        < (June_12_2020
        * 1000),
        true
    ),
    gleam@should:equal(
        gleam@os:system_time(millisecond)
        > (June_12_2020
        * 1000),
        true
    ),
    gleam@should:equal(
        gleam@os:system_time(millisecond)
        < (June_12_2020
        * 1000000),
        true
    ).

erlang_timestamp_test() ->
    June_12_2020 = 1591966971000000,
    {Mega_seconds, Seconds, Micro_seconds} = gleam@os:erlang_timestamp(),
    Stamp_as_micro = (((Mega_seconds * 1000000) + Seconds) * 1000000) + Micro_seconds,
    gleam@should:be_true(Stamp_as_micro > June_12_2020).
