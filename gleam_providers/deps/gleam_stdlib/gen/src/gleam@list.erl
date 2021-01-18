-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, head/1, tail/1, filter/2, filter_map/2, map/2, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, flatten/1, fold/3, fold_right/3, index_fold/3, try_fold/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, partition/2, permutations/1]).

length(A) ->
    erlang:length(A).

reverse(A) ->
    lists:reverse(A).

is_empty(List) ->
    List =:= [].

contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | Rest] ->
            (Head =:= Elem) orelse contains(Rest, Elem)
    end.

head(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

tail(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

filter(List, Predicate) ->
    do_filter(List, Predicate, []).

do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

do_map(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

map(List, Fun) ->
    do_map(List, Fun, []).

do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [X | Xs] ->
            do_index_map(Xs, Fun, Index + 1, [Fun(Index, X) | Acc])
    end.

index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

try_map(List, Fun) ->
    do_try_map(List, Fun, []).

drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

take(List, N) ->
    do_take(List, N, []).

new() ->
    [].

append(A, B) ->
    lists:append(A, B).

do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            Acc;

        [L | Rest] ->
            do_flatten(Rest, lists:append(Acc, L))
    end.

flatten(Lists) ->
    do_flatten(Lists, []).

fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(X, Initial), Fun)
    end.

fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(X, fold_right(Rest, Initial, Fun))
    end.

do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Index, First, Acc), With, Index + 1)
    end.

index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(First, Accumulator) of
                {ok, Next_accumulator} ->
                    try_fold(Rest, Next_accumulator, Fun);

                {error, Err} ->
                    {error, Err}
            end
    end.

find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

all(List, Predicate) ->
    case List of
        [] ->
            true;

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    all(Rest, Predicate);

                _ ->
                    false
            end
    end.

any(List, Predicate) ->
    case List of
        [] ->
            false;

        [X | Rest] ->
            case Predicate(X) of
                false ->
                    any(Rest, Predicate);

                _ ->
                    true
            end
    end.

zip(Xs, Ys) ->
    case {Xs, Ys} of
        {[], _} ->
            [];

        {_, []} ->
            [];

        {[X | Xs@1], [Y | Ys@1]} ->
            [{X, Y} | zip(Xs@1, Ys@1)]
    end.

strict_zip(L1, L2) ->
    case erlang:length(L1) =:= erlang:length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {lists:reverse(Xs), lists:reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

unzip(Input) ->
    do_unzip(Input, [], []).

intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            [X, Elem | intersperse(Rest, Elem)]
    end.

at(List, Index) ->
    case Index < 0 of
        true ->
            {error, nil};

        false ->
            case List of
                [] ->
                    {error, nil};

                [X | Rest] ->
                    case Index =:= 0 of
                        true ->
                            {ok, X};

                        false ->
                            at(Rest, Index - 1)
                    end
            end
    end.

unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

merge_sort(A, B, Compare) ->
    case {A, B} of
        {[], _} ->
            B;

        {_, []} ->
            A;

        {[Ax | Ar], [Bx | Br]} ->
            case Compare(Ax, Bx) of
                lt ->
                    [Ax | merge_sort(Ar, B, Compare)];

                _ ->
                    [Bx | merge_sort(A, Br, Compare)]
            end
    end.

do_sort(List, Compare, List_length) ->
    case List_length < 2 of
        true ->
            List;

        false ->
            Split_length = List_length div 2,
            A_list = take(List, Split_length),
            B_list = drop(List, Split_length),
            merge_sort(
                do_sort(A_list, Compare, Split_length),
                do_sort(B_list, Compare, List_length - Split_length),
                Compare
            )
    end.

sort(List, Compare) ->
    do_sort(List, Compare, erlang:length(List)).

range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [];

        gt ->
            [Start | range(Start - 1, Stop)];

        lt ->
            [Start | range(Start + 1, Stop)]
    end.

do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

repeat(A, Times) ->
    do_repeat(A, Times, []).

do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

split(List, Index) ->
    do_split(List, Index, []).

do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {lists:reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

key_find(Keyword_list, Desired_key) ->
    find_map(Keyword_list, fun(Keyword) -> {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end end).

do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, lists:append(lists:reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, lists:append(lists:reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

key_pop(Haystack, Key) ->
    pop_map(Haystack, fun(Entry) -> {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end end).

key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            flatten(
                map(
                    L,
                    fun(X) ->
                        map(
                            permutations(filter(L, fun(Y) -> Y /= X end)),
                            fun(Gleam@capture_variable) ->
                                lists:append([X], Gleam@capture_variable)
                            end
                        )
                    end
                )
            )
    end.
