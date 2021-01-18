-module(gleam@int).
-compile(no_auto_import).

-export([absolute_value/1, parse/1, to_string/1, to_base_string/2, to_float/1, compare/2, min/2, max/2, is_even/1, is_odd/1, negate/1, sum/1, product/1]).

absolute_value(Num) ->
    case Num >= 0 of
        true ->
            Num;

        false ->
            Num * -1
    end.

parse(A) ->
    gleam_stdlib:parse_int(A).

to_string(A) ->
    erlang:integer_to_binary(A).

to_base_string(A, B) ->
    erlang:integer_to_binary(A, B).

to_float(A) ->
    erlang:float(A).

compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        false ->
            case A < B of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

is_even(X) ->
    (X rem 2) =:= 0.

is_odd(X) ->
    (X rem 2) /= 0.

negate(X) ->
    -1 * X.

sum(Numbers) ->
    do_sum(Numbers, 0).

do_sum(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_sum(Rest, X + Initial)
    end.

product(Numbers) ->
    case Numbers of
        [] ->
            0;

        _ ->
            do_product(Numbers, 1)
    end.

do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.
