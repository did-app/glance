-module(gleam@float).
-compile(no_auto_import).

-export([parse/1, to_string/1, compare/2, min/2, max/2, ceiling/1, floor/1, round/1, truncate/1, absolute_value/1, power/2, square_root/1, negate/1, sum/1, product/1]).

parse(A) ->
    gleam_stdlib:parse_float(A).

to_string(F) ->
    gleam@string_builder:to_string(gleam@string_builder:from_float(F)).

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

ceiling(A) ->
    math:ceil(A).

floor(A) ->
    math:floor(A).

round(A) ->
    erlang:round(A).

truncate(A) ->
    erlang:trunc(A).

absolute_value(A) ->
    erlang:abs(A).

power(A, B) ->
    math:pow(A, B).

square_root(Number) ->
    case Number < 0.0 of
        true ->
            {error, nil};

        false ->
            {ok, math:pow(Number, 0.5)}
    end.

negate(X) ->
    -1.0 * X.

sum(Numbers) ->
    do_sum(Numbers, 0.0).

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
            0.0;

        _ ->
            do_product(Numbers, 1.0)
    end.

do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.
