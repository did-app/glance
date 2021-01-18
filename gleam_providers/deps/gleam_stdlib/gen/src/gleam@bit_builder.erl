-module(gleam@bit_builder).
-compile(no_auto_import).

-export([prepend/2, append/2, prepend_builder/2, append_builder/2, prepend_string/2, append_string/2, concat/1, from_string/1, from_string_builder/1, from_bit_string/1, to_bit_string/1, byte_size/1]).

prepend(A, B) ->
    gleam_stdlib:iodata_prepend(A, B).

append(A, B) ->
    gleam_stdlib:iodata_append(A, B).

prepend_builder(A, B) ->
    gleam_stdlib:iodata_prepend(A, B).

append_builder(A, B) ->
    gleam_stdlib:iodata_append(A, B).

prepend_string(A, B) ->
    gleam_stdlib:iodata_prepend(A, B).

append_string(A, B) ->
    gleam_stdlib:iodata_append(A, B).

concat(A) ->
    gleam_stdlib:identity(A).

from_string(A) ->
    gleam_stdlib:wrap_list(A).

from_string_builder(A) ->
    gleam_stdlib:identity(A).

from_bit_string(A) ->
    gleam_stdlib:wrap_list(A).

to_bit_string(A) ->
    erlang:list_to_bitstring(A).

byte_size(A) ->
    erlang:iolist_size(A).
