-module(gleam@option).
-compile(no_auto_import).

-export([is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, map/2, flatten/1, then/2, 'or'/2]).

is_some(Option) ->
    Option /= none.

is_none(Option) ->
    Option =:= none.

to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.
