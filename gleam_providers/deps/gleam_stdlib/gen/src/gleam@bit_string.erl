-module(gleam@bit_string).
-compile(no_auto_import).

-export([from_string/1, byte_size/1, append/2, part/3, int_to_u32/1, int_from_u32/1, is_utf8/1, to_string/1]).

from_string(A) ->
    gleam_stdlib:identity(A).

byte_size(A) ->
    erlang:byte_size(A).

append(A, B) ->
    gleam_stdlib:bit_string_append(A, B).

part(A, B, C) ->
    gleam_stdlib:bit_string_part_(A, B, C).

int_to_u32(A) ->
    gleam_stdlib:bit_string_int_to_u32(A).

int_from_u32(A) ->
    gleam_stdlib:bit_string_int_from_u32(A).

is_utf8(Bits) ->
    case Bits of
        <<>> ->
            true;

        <<_/utf8, Rest/binary>> ->
            is_utf8(Rest);

        _ ->
            false
    end.

to_string(Bits) ->
    case is_utf8(Bits) of
        true ->
            {ok, gleam_stdlib:identity(Bits)};

        false ->
            {error, nil}
    end.
