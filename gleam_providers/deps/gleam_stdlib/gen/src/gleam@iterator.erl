-module(gleam@iterator).
-compile(no_auto_import).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, append/2, flatten/1, flat_map/2, filter/2, cycle/1, range/2, find/2]).

do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

unfold(Initial, F) ->
    {iterator, do_unfold(Initial, F)}.

repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

repeat(X) ->
    repeatedly(fun() -> X end).

from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

do_fold(Continuation, Initial, F) ->
    case Continuation() of
        {continue, Element, Iterator} ->
            do_fold(Iterator, F(Element, Initial), F);

        stop ->
            Initial
    end.

fold(Iterator, Initial, F) ->
    do_fold(erlang:element(2, Iterator), Initial, F).

run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

to_list(Iterator) ->
    gleam@list:reverse(fold(Iterator, [], fun(E, Acc) -> [E | Acc] end)).

step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

do_take(Continuation, Desired, Acc) ->
    case Desired > 0 of
        true ->
            case Continuation() of
                {continue, Element, Iterator} ->
                    do_take(Iterator, Desired - 1, [Element | Acc]);

                stop ->
                    gleam@list:reverse(Acc)
            end;

        false ->
            gleam@list:reverse(Acc)
    end.

take(Iterator, Desired) ->
    do_take(erlang:element(2, Iterator), Desired, []).

do_drop(Continuation, Desired) ->
    case Desired > 0 of
        true ->
            case Continuation() of
                {continue, _, Iterator} ->
                    do_drop(Iterator, Desired - 1);

                stop ->
                    fun() -> stop end
            end;

        false ->
            Continuation
    end.

drop(Iterator, Desired) ->
    {iterator, do_drop(erlang:element(2, Iterator), Desired)}.

do_map(Continuation, F) ->
    fun() -> case Continuation() of
            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)};

            stop ->
                stop
        end end.

map(Iterator, F) ->
    {iterator, do_map(erlang:element(2, Iterator), F)}.

do_append(First, Second) ->
    fun() -> case First() of
            {continue, E, First@1} ->
                {continue, E, do_append(First@1, Second)};

            stop ->
                Second()
        end end.

append(First, Second) ->
    {iterator, do_append(erlang:element(2, First), erlang:element(2, Second))}.

do_flatten(Continuation) ->
    fun() -> case Continuation() of
            {continue, E, Continuation@1} ->
                (do_append(erlang:element(2, E), do_flatten(Continuation@1)))();

            stop ->
                stop
        end end.

flatten(Iterator) ->
    {iterator, do_flatten(erlang:element(2, Iterator))}.

flat_map(Iterator, F) ->
    flatten(map(Iterator, F)).

do_filter(Continuation, Predicate) ->
    fun() -> case Continuation() of
            {continue, E, Iterator} ->
                case Predicate(E) of
                    true ->
                        {continue, E, do_filter(Iterator, Predicate)};

                    false ->
                        (do_filter(Iterator, Predicate))()
                end;

            stop ->
                stop
        end end.

filter(Iterator, Predicate) ->
    {iterator, do_filter(erlang:element(2, Iterator), Predicate)}.

do_cycle(Next, Reset) ->
    fun() -> case Next() of
            {continue, E, Iterator} ->
                {continue, E, do_cycle(Iterator, Reset)};

            stop ->
                (do_cycle(Reset, Reset))()
        end end.

cycle(Iterator) ->
    {iterator,
     do_cycle(erlang:element(2, Iterator), erlang:element(2, Iterator))}.

do_range(Current, Limit, Inc) ->
    case Current =:= Limit of
        true ->
            fun() -> stop end;

        false ->
            fun() ->
                {continue, Current, do_range(Current + Inc, Limit, Inc)}
            end
    end.

range(Start, Stop) ->
    {iterator, do_range(Start, Stop, case Start < Stop of
             true ->
                 1;

             false ->
                 -1
         end)}.

find(Haystack, Is_desired) ->
    case (erlang:element(2, Haystack))() of
        {continue, Element, Continuation} ->
            case Is_desired(Element) of
                true ->
                    {ok, Element};

                false ->
                    find({iterator, Continuation}, Is_desired)
            end;

        stop ->
            {error, nil}
    end.
