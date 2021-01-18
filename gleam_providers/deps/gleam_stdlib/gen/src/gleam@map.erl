-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).

size(A) ->
    maps:size(A).

to_list(A) ->
    maps:to_list(A).

from_list(A) ->
    maps:from_list(A).

has_key(Map, Key) ->
    maps:is_key(Key, Map).

new() ->
    maps:new().

get(A, B) ->
    gleam_stdlib:map_get(A, B).

insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

map_values(Map, Fun) ->
    maps:map(Fun, Map).

keys(A) ->
    maps:keys(A).

values(A) ->
    maps:values(A).

filter(Map, Property) ->
    maps:filter(Property, Map).

take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

merge(A, B) ->
    maps:merge(A, B).

delete(Map, Key) ->
    maps:remove(Key, Map).

drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun(Key, Acc) -> delete(Acc, Key) end).

update(Map, Key, Fun) ->
    insert(Map, Key, Fun(gleam_stdlib:map_get(Map, Key))).

do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(K, V, Initial), Fun)
    end.

fold(Map, Initial, Fun) ->
    do_fold(maps:to_list(Map), Initial, Fun).
