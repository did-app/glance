-module(gleam@set).
-compile(no_auto_import).

-export([new/0, size/1, insert/2, contains/2, delete/2, to_list/1, from_list/1, fold/3, filter/2, take/2, union/2, intersection/2]).

new() ->
    {set, gleam@map:new()}.

size(Set) ->
    gleam@map:size(erlang:element(2, Set)).

insert(Set, Member) ->
    {set, gleam@map:insert(erlang:element(2, Set), Member, [])}.

contains(Set, Member) ->
    gleam@result:is_ok(gleam@map:get(erlang:element(2, Set), Member)).

delete(Set, Member) ->
    {set, gleam@map:delete(erlang:element(2, Set), Member)}.

to_list(Set) ->
    gleam@map:keys(erlang:element(2, Set)).

from_list(Members) ->
    Map = gleam@list:fold(
        Members,
        gleam@map:new(),
        fun(K, M) -> gleam@map:insert(M, K, []) end
    ),
    {set, Map}.

fold(Set, Initial, Reducer) ->
    gleam@map:fold(
        erlang:element(2, Set),
        Initial,
        fun(K, _, A) -> Reducer(K, A) end
    ).

filter(Set, Property) ->
    {set,
     gleam@map:filter(erlang:element(2, Set), fun(M, _) -> Property(M) end)}.

take(Set, Desired) ->
    {set, gleam@map:take(erlang:element(2, Set), Desired)}.

order(First, Second) ->
    case gleam@map:size(erlang:element(2, First))
    > gleam@map:size(erlang:element(2, Second)) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun(M, A) -> insert(A, M) end).

intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).
