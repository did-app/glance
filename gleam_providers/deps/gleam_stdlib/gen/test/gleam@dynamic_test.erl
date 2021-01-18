-module(gleam@dynamic_test).
-compile(no_auto_import).

-export([bit_string_test/0, string_test/0, int_test/0, float_test/0, thunk_test/0, bool_test/0, atom_test/0, typed_list_test/0, option_test/0, field_test/0, element_test/0, tuple2_test/0, typed_tuple2_test/0, map_test/0, list_test/0, result_test/0, typed_result_test/0]).

bit_string_test() ->
    gleam@should:equal(
        gleam@dynamic:bit_string(gleam@dynamic:from(<<""/utf8>>)),
        {ok, <<""/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:bit_string(gleam@dynamic:from(<<"Hello"/utf8>>)),
        {ok, <<"Hello"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:bit_string(gleam@dynamic:from(<<65535:16>>)),
        {ok, <<65535:16>>}
    ),
    gleam@should:equal(
        gleam@dynamic:bit_string(gleam@dynamic:from(1)),
        {error, <<"Expected a bit_string, got an int"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:bit_string(gleam@dynamic:from([])),
        {error, <<"Expected a bit_string, got a list"/utf8>>}
    ).

string_test() ->
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(<<""/utf8>>)),
        {ok, <<""/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(<<"Hello"/utf8>>)),
        {ok, <<"Hello"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(<<65535:16>>)),
        {error, <<"Expected a string, got a bit_string"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from(1)),
        {error, <<"Expected a bit_string, got an int"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:string(gleam@dynamic:from([])),
        {error, <<"Expected a bit_string, got a list"/utf8>>}
    ).

int_test() ->
    gleam@should:equal(gleam@dynamic:int(gleam@dynamic:from(1)), {ok, 1}),
    gleam@should:equal(gleam@dynamic:int(gleam@dynamic:from(2)), {ok, 2}),
    gleam@should:equal(
        gleam@dynamic:int(gleam@dynamic:from(1.0)),
        {error, <<"Expected an int, got a float"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:int(gleam@dynamic:from([])),
        {error, <<"Expected an int, got a list"/utf8>>}
    ).

float_test() ->
    gleam@should:equal(gleam@dynamic:float(gleam@dynamic:from(1.0)), {ok, 1.0}),
    gleam@should:equal(gleam@dynamic:float(gleam@dynamic:from(2.2)), {ok, 2.2}),
    gleam@should:equal(
        gleam@dynamic:float(gleam@dynamic:from(1)),
        {error, <<"Expected a float, got an int"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:float(gleam@dynamic:from([])),
        {error, <<"Expected a float, got a list"/utf8>>}
    ).

thunk_test() ->
    gleam@should:be_ok(gleam@dynamic:thunk(gleam@dynamic:from(fun() -> 1 end))),
    gleam@should:equal(
        gleam@result:map(
            gleam@dynamic:thunk(gleam@dynamic:from(fun() -> 1 end)),
            fun(F) -> F() end
        ),
        {ok, gleam@dynamic:from(1)}
    ),
    gleam@should:be_error(
        gleam@dynamic:thunk(gleam@dynamic:from(fun(X) -> X end))
    ),
    gleam@should:be_error(gleam@dynamic:thunk(gleam@dynamic:from(1))),
    gleam@should:be_error(gleam@dynamic:thunk(gleam@dynamic:from([]))).

bool_test() ->
    gleam@should:equal(gleam@dynamic:bool(gleam@dynamic:from(true)), {ok, true}),
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from(false)),
        {ok, false}
    ),
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from(1)),
        {error, <<"Expected a bool, got an int"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:bool(gleam@dynamic:from([])),
        {error, <<"Expected a bool, got a list"/utf8>>}
    ).

atom_test() ->
    gleam@should:equal(
        gleam@dynamic:atom(
            gleam@dynamic:from(gleam@atom:create_from_string(<<""/utf8>>))
        ),
        {ok, gleam@atom:create_from_string(<<""/utf8>>)}
    ),
    gleam@should:equal(
        gleam@dynamic:atom(
            gleam@dynamic:from(gleam@atom:create_from_string(<<"ok"/utf8>>))
        ),
        {ok, gleam@atom:create_from_string(<<"ok"/utf8>>)}
    ),
    gleam@should:be_error(gleam@dynamic:atom(gleam@dynamic:from(1))),
    gleam@should:be_error(gleam@dynamic:atom(gleam@dynamic:from([]))).

typed_list_test() ->
    gleam@should:equal(
        gleam@dynamic:typed_list(
            gleam@dynamic:from([]),
            fun gleam@dynamic:string/1
        ),
        {ok, []}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_list(
            gleam@dynamic:from([]),
            fun gleam@dynamic:int/1
        ),
        {ok, []}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_list(
            gleam@dynamic:from([1, 2, 3]),
            fun gleam@dynamic:int/1
        ),
        {ok, [1, 2, 3]}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_list(
            gleam@dynamic:from([[1], [2], [3]]),
            fun(Gleam@capture_variable) ->
                gleam@dynamic:typed_list(
                    Gleam@capture_variable,
                    fun gleam@dynamic:int/1
                )
            end
        ),
        {ok, [[1], [2], [3]]}
    ),
    gleam@should:be_error(
        gleam@dynamic:typed_list(
            gleam@dynamic:from(1),
            fun gleam@dynamic:string/1
        )
    ),
    gleam@should:be_error(
        gleam@dynamic:typed_list(
            gleam@dynamic:from(1.0),
            fun gleam@dynamic:int/1
        )
    ),
    gleam@should:be_error(
        gleam@dynamic:typed_list(
            gleam@dynamic:from([<<""/utf8>>]),
            fun gleam@dynamic:int/1
        )
    ),
    gleam@should:be_error(
        gleam@dynamic:typed_list(
            gleam@dynamic:from(
                [gleam@dynamic:from(1),
                 gleam@dynamic:from(<<"not an int"/utf8>>)]
            ),
            fun gleam@dynamic:int/1
        )
    ).

option_test() ->
    {ok, Null} = gleam@atom:from_string(<<"null"/utf8>>),
    gleam@should:equal(
        gleam@dynamic:option(gleam@dynamic:from(1), fun gleam@dynamic:int/1),
        {ok, {some, 1}}
    ),
    gleam@should:equal(
        gleam@dynamic:option(gleam@dynamic:from(Null), fun gleam@dynamic:int/1),
        {ok, none}
    ),
    gleam@should:be_error(
        gleam@dynamic:option(gleam@dynamic:from(1), fun gleam@dynamic:string/1)
    ).

field_test() ->
    {ok, Ok_atom} = gleam@atom:from_string(<<"ok"/utf8>>),
    {ok, Error_atom} = gleam@atom:from_string(<<"error"/utf8>>),
    gleam@should:equal(
        gleam@dynamic:field(
            gleam@dynamic:from(gleam@map:insert(gleam@map:new(), Ok_atom, 1)),
            Ok_atom
        ),
        {ok, gleam@dynamic:from(1)}
    ),
    gleam@should:equal(
        gleam@dynamic:field(
            gleam@dynamic:from(
                gleam@map:insert(
                    gleam@map:insert(gleam@map:new(), Ok_atom, 3),
                    Error_atom,
                    1
                )
            ),
            Ok_atom
        ),
        {ok, gleam@dynamic:from(3)}
    ),
    gleam@should:be_error(
        gleam@dynamic:field(gleam@dynamic:from(gleam@map:new()), Ok_atom)
    ),
    gleam@should:be_error(gleam@dynamic:field(gleam@dynamic:from(1), Ok_atom)),
    gleam@should:be_error(gleam@dynamic:field(gleam@dynamic:from([]), [])).

element_test() ->
    {ok, Ok_atom} = gleam@atom:from_string(<<"ok"/utf8>>),
    Ok_one_tuple = {Ok_atom, 1},
    gleam@should:equal(
        gleam@dynamic:element(gleam@dynamic:from(Ok_one_tuple), 0),
        {ok, gleam@dynamic:from(Ok_atom)}
    ),
    gleam@should:equal(
        gleam@dynamic:element(gleam@dynamic:from(Ok_one_tuple), 1),
        {ok, gleam@dynamic:from(1)}
    ),
    gleam@should:be_error(
        gleam@dynamic:element(gleam@dynamic:from(Ok_one_tuple), 2)
    ),
    gleam@should:be_error(
        gleam@dynamic:element(gleam@dynamic:from(Ok_one_tuple), -1)
    ),
    gleam@should:be_error(gleam@dynamic:element(gleam@dynamic:from(1), 0)),
    gleam@should:be_error(
        gleam@dynamic:element(
            gleam@dynamic:from(gleam@map:insert(gleam@map:new(), 1, Ok_atom)),
            0
        )
    ).

tuple2_test() ->
    gleam@should:equal(
        gleam@dynamic:tuple2(gleam@dynamic:from({1, 2})),
        {ok, {gleam@dynamic:from(1), gleam@dynamic:from(2)}}
    ),
    gleam@should:equal(
        gleam@dynamic:tuple2(gleam@dynamic:from({1, <<""/utf8>>})),
        {ok, {gleam@dynamic:from(1), gleam@dynamic:from(<<""/utf8>>)}}
    ),
    gleam@should:equal(
        gleam@dynamic:tuple2(gleam@dynamic:from({1, 2, 3})),
        {error, <<"Expected a 2 element tuple, got a 3 element tuple"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:tuple2(gleam@dynamic:from(1)),
        {error, <<"Expected a 2 element tuple, got an int"/utf8>>}
    ).

typed_tuple2_test() ->
    gleam@should:equal(
        gleam@dynamic:typed_tuple2(
            gleam@dynamic:from({1, 2}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:int/1
        ),
        {ok, {1, 2}}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_tuple2(
            gleam@dynamic:from({1, <<""/utf8>>}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:string/1
        ),
        {ok, {1, <<""/utf8>>}}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_tuple2(
            gleam@dynamic:from({1, <<""/utf8>>}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:int/1
        ),
        {error, <<"Expected an int, got a binary"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_tuple2(
            gleam@dynamic:from({1, 2, 3}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:int/1
        ),
        {error, <<"Expected a 2 element tuple, got a 3 element tuple"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_tuple2(
            gleam@dynamic:from(1),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:int/1
        ),
        {error, <<"Expected a 2 element tuple, got an int"/utf8>>}
    ).

map_test() ->
    gleam@should:equal(
        gleam@dynamic:map(gleam@dynamic:from(gleam@map:new())),
        {ok, gleam@map:new()}
    ),
    gleam@should:equal(
        gleam@dynamic:map(gleam@dynamic:from(1)),
        {error, <<"Expected a map, got an int"/utf8>>}
    ).

list_test() ->
    gleam@should:equal(gleam@dynamic:list(gleam@dynamic:from([])), {ok, []}),
    gleam@should:equal(
        gleam@dynamic:list(gleam@dynamic:from([1, 2])),
        {ok, [gleam@dynamic:from(1), gleam@dynamic:from(2)]}
    ),
    gleam@should:equal(
        gleam@dynamic:list(
            gleam@dynamic:from([gleam@dynamic:from(1), gleam@dynamic:from(2.0)])
        ),
        {ok, [gleam@dynamic:from(1), gleam@dynamic:from(2.0)]}
    ),
    gleam@should:equal(
        gleam@dynamic:list(gleam@dynamic:from(1)),
        {error, <<"Expected a list, got an int"/utf8>>}
    ).

result_test() ->
    gleam@should:equal(
        gleam@dynamic:result(gleam@dynamic:from({ok, 1})),
        {ok, {ok, gleam@dynamic:from(1)}}
    ),
    gleam@should:equal(
        gleam@dynamic:result(gleam@dynamic:from({error, <<"error"/utf8>>})),
        {ok, {error, gleam@dynamic:from(<<"error"/utf8>>)}}
    ),
    gleam@should:equal(
        gleam@dynamic:result(gleam@dynamic:from(1)),
        {error, <<"Expected a 2 element tuple, got an int"/utf8>>}
    ),
    Tag = gleam@atom:create_from_string(<<"bad"/utf8>>),
    gleam@should:equal(
        gleam@dynamic:result(gleam@dynamic:from({Tag, <<"value"/utf8>>})),
        {error, <<"Expected a tag of \"ok\" or \"error\", got \"bad\""/utf8>>}
    ).

typed_result_test() ->
    gleam@should:equal(
        gleam@dynamic:typed_result(
            gleam@dynamic:from({ok, 1}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:string/1
        ),
        {ok, {ok, 1}}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_result(
            gleam@dynamic:from({error, <<"error"/utf8>>}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:string/1
        ),
        {ok, {error, <<"error"/utf8>>}}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_result(
            gleam@dynamic:from({ok, <<"1"/utf8>>}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:string/1
        ),
        {error, <<"Expected an int, got a binary"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_result(
            gleam@dynamic:from({error, 1}),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:string/1
        ),
        {error, <<"Expected a bit_string, got an int"/utf8>>}
    ),
    gleam@should:equal(
        gleam@dynamic:typed_result(
            gleam@dynamic:from(1),
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:string/1
        ),
        {error, <<"Expected a 2 element tuple, got an int"/utf8>>}
    ).
