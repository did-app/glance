-module(gleam@bool).
-compile(no_auto_import).

-export([negate/1, nor/2, nand/2, exclusive_or/2, exclusive_nor/2, compare/2, max/2, min/2, to_int/1]).

negate(Bool) ->
    case Bool of
        true ->
            false;

        false ->
            true
    end.

nor(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            false;

        {true, false} ->
            false;

        {true, true} ->
            false
    end.

nand(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            true;

        {true, false} ->
            true;

        {true, true} ->
            false
    end.

exclusive_or(A, B) ->
    case {A, B} of
        {false, false} ->
            false;

        {false, true} ->
            true;

        {true, false} ->
            true;

        {true, true} ->
            false
    end.

exclusive_nor(A, B) ->
    case {A, B} of
        {false, false} ->
            true;

        {false, true} ->
            false;

        {true, false} ->
            false;

        {true, true} ->
            true
    end.

compare(A, B) ->
    case {A, B} of
        {true, true} ->
            eq;

        {true, false} ->
            gt;

        {false, false} ->
            eq;

        {false, true} ->
            lt
    end.

max(A, B) ->
    case A of
        true ->
            true;

        false ->
            B
    end.

min(A, B) ->
    case A of
        false ->
            false;

        true ->
            B
    end.

to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.
