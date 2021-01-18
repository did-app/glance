-module(gleam@dynamic).
-compile(no_auto_import).

-export([from/1, unsafe_coerce/1, bit_string/1, string/1, int/1, float/1, atom/1, bool/1, thunk/1, list/1, result/1, typed_result/3, typed_list/2, option/2, field/2, element/2, tuple2/1, typed_tuple2/3, map/1, any/2]).

from(A) ->
    gleam_stdlib:identity(A).

unsafe_coerce(A) ->
    gleam_stdlib:identity(A).

bit_string(A) ->
    gleam_stdlib:decode_bit_string(A).

string(From) ->
    gleam@result:then(
        gleam_stdlib:decode_bit_string(From),
        fun(Raw) -> case gleam@bit_string:to_string(Raw) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error, <<"Expected a string, got a bit_string"/utf8>>}
            end end
    ).

int(A) ->
    gleam_stdlib:decode_int(A).

float(A) ->
    gleam_stdlib:decode_float(A).

atom(A) ->
    gleam_stdlib:decode_atom(A).

bool(A) ->
    gleam_stdlib:decode_bool(A).

thunk(A) ->
    gleam_stdlib:decode_thunk(A).

list(A) ->
    gleam_stdlib:decode_list(A).

result(From) ->
    case gleam_stdlib:decode_tuple2(From) of
        {error, Gleam@try_error} -> {error, Gleam@try_error};
        {ok, {Key, Val}} ->
            case gleam_stdlib:decode_atom(Key) of
                {error, Gleam@try_error@1} -> {error, Gleam@try_error@1};
                {ok, Tag} ->
                    Ok_atom = gleam@atom:create_from_string(<<"ok"/utf8>>),
                    Error_atom = gleam@atom:create_from_string(<<"error"/utf8>>),
                    case Tag of
                        Tag@1 when Tag@1 =:= Ok_atom ->
                            {ok, {ok, Val}};

                        Tag@2 when Tag@2 =:= Error_atom ->
                            {ok, {error, Val}};

                        Tag@3 ->
                            {error,
                             gleam@string_builder:to_string(
                                 gleam@string_builder:append(
                                     gleam@string_builder:append(
                                         gleam@string_builder:from_string(
                                             <<"Expected a tag of \"ok\" or \"error\", got \""/utf8>>
                                         ),
                                         gleam@atom:to_string(Tag@3)
                                     ),
                                     <<"\""/utf8>>
                                 )
                             )}
                    end
            end
    end.

typed_result(Dynamic, Decode_ok, Decode_error) ->
    case result(Dynamic) of
        {error, Gleam@try_error} -> {error, Gleam@try_error};
        {ok, Inner_result} ->
            case Inner_result of
                {ok, Raw} ->
                    gleam@result:map(Decode_ok(Raw), fun(A) -> {ok, A} end);

                {error, Raw@1} ->
                    gleam@result:map(
                        Decode_error(Raw@1),
                        fun(A) -> {error, A} end
                    )
            end
    end.

typed_list(Dynamic, Decoder_type) ->
    gleam@result:then(
        gleam_stdlib:decode_list(Dynamic),
        fun(Gleam@capture_variable) ->
            gleam@list:try_map(Gleam@capture_variable, Decoder_type)
        end
    ).

option(Dynamic, Decoder) ->
    {ok, Null} = gleam@atom:from_string(<<"null"/utf8>>),
    case {gleam_stdlib:decode_atom(Dynamic), Decoder(Dynamic)} of
        {{ok, Atom}, _} when Atom =:= Null ->
            {ok, none};

        {_, {ok, Result}} ->
            {ok, {some, Result}};

        {_, {error, Msg}} ->
            {error, Msg}
    end.

field(A, B) ->
    gleam_stdlib:decode_field(A, B).

element(A, B) ->
    gleam_stdlib:decode_element(A, B).

tuple2(A) ->
    gleam_stdlib:decode_tuple2(A).

typed_tuple2(Tup, Decode_first, Decode_second) ->
    case gleam_stdlib:decode_tuple2(Tup) of
        {error, Gleam@try_error} -> {error, Gleam@try_error};
        {ok, {First, Second}} ->
            case Decode_first(First) of
                {error, Gleam@try_error@1} -> {error, Gleam@try_error@1};
                {ok, A} ->
                    case Decode_second(Second) of
                        {error, Gleam@try_error@2} -> {error, Gleam@try_error@2};
                        {ok, B} ->
                            {ok, {A, B}}
                    end
            end
    end.

map(A) ->
    gleam_stdlib:decode_map(A).

any(Data, Decoders) ->
    gleam@result:map_error(
        gleam@list:find_map(Decoders, fun(Decoder) -> Decoder(Data) end),
        fun(_) -> <<"Unexpected value"/utf8>> end
    ).
