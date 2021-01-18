-module(gleam@os).
-compile(no_auto_import).

-export([get_env/0, insert_env/2, delete_env/1, system_time/1, erlang_timestamp/0]).

get_env() ->
    gleam@map:from_list(
        gleam@list:map(
            os:getenv(),
            fun(Char_list) ->
                {ok, Value} = gleam@string:split_once(
                    erlang:list_to_binary(Char_list),
                    <<"="/utf8>>
                ),
                Value
            end
        )
    ).

insert_env(Key, Value) ->
    os:putenv(erlang:binary_to_list(Key), erlang:binary_to_list(Value)),
    nil.

delete_env(Key) ->
    os:unsetenv(erlang:binary_to_list(Key)),
    nil.

system_time(A) ->
    os:system_time(A).

erlang_timestamp() ->
    os:timestamp().
