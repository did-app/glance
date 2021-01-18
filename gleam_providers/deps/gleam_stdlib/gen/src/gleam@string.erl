-module(gleam@string).
-compile(no_auto_import).

-export([is_empty/1, length/1, reverse/1, replace/3, lowercase/1, uppercase/1, compare/2, slice/3, drop_left/2, drop_right/2, contains/2, starts_with/2, ends_with/2, split/2, split_once/2, append/2, concat/1, repeat/2, join/2, pad_left/3, pad_right/3, trim/1, trim_left/1, trim_right/1, pop_grapheme/1, to_graphemes/1, utf_codepoint/1]).

is_empty(Str) ->
    Str =:= <<""/utf8>>.

length(A) ->
    string:length(A).

reverse(String) ->
    gleam@string_builder:to_string(
        gleam@string_builder:reverse(gleam@string_builder:from_string(String))
    ).

replace(String, Pattern, Substitute) ->
    gleam@string_builder:to_string(
        gleam@string_builder:replace(
            gleam@string_builder:from_string(String),
            Pattern,
            Substitute
        )
    ).

lowercase(A) ->
    string:lowercase(A).

uppercase(A) ->
    string:uppercase(A).

compare(A, B) ->
    gleam_stdlib:compare_strings(A, B).

slice(String, Idx, Len) ->
    case Len < 0 of
        true ->
            <<""/utf8>>;

        false ->
            case Idx < 0 of
                true ->
                    Translated_idx = string:length(String) + Idx,
                    case Translated_idx < 0 of
                        true ->
                            <<""/utf8>>;

                        false ->
                            string:slice(String, Translated_idx, Len)
                    end;

                false ->
                    string:slice(String, Idx, Len)
            end
    end.

drop_left(String, Num_graphemes) ->
    case Num_graphemes < 0 of
        true ->
            String;

        false ->
            slice(String, Num_graphemes, string:length(String) - Num_graphemes)
    end.

drop_right(String, Num_graphemes) ->
    case Num_graphemes < 0 of
        true ->
            String;

        false ->
            slice(String, 0, string:length(String) - Num_graphemes)
    end.

contains(Haystack, Needle) ->
    gleam@result:is_error(gleam@dynamic:atom(string:find(Haystack, Needle))).

starts_with(A, B) ->
    gleam_stdlib:string_starts_with(A, B).

ends_with(A, B) ->
    gleam_stdlib:string_ends_with(A, B).

split(X, Substring) ->
    gleam@list:map(
        gleam@string_builder:split(
            gleam@string_builder:from_string(X),
            Substring
        ),
        fun gleam@string_builder:to_string/1
    ).

split_once(X, Substring) ->
    case string:split(X, Substring) of
        [First, Rest] ->
            {ok, {First, Rest}};

        _ ->
            {error, nil}
    end.

append(First, Second) ->
    gleam@string_builder:to_string(
        gleam@string_builder:append(
            gleam@string_builder:from_string(First),
            Second
        )
    ).

concat(Strings) ->
    gleam@string_builder:to_string(gleam@string_builder:from_strings(Strings)).

repeat(String, Times) ->
    concat(gleam@iterator:take(gleam@iterator:repeat(String), Times)).

join(Strings, Separator) ->
    concat(gleam@list:intersperse(Strings, Separator)).

pad_left(String, Length, Pad_string) ->
    gleam_stdlib:string_pad(String, Length, leading, Pad_string).

pad_right(String, Length, Pad_string) ->
    gleam_stdlib:string_pad(String, Length, trailing, Pad_string).

trim(String) ->
    string:trim(String, both).

trim_left(String) ->
    string:trim(String, leading).

trim_right(String) ->
    string:trim(String, trailing).

pop_grapheme(A) ->
    gleam_stdlib:string_pop_grapheme(A).

to_graphemes(String) ->
    case gleam_stdlib:string_pop_grapheme(String) of
        {ok, {Grapheme, Rest}} ->
            [Grapheme | to_graphemes(Rest)];

        _ ->
            []
    end.

utf_codepoint(Value) ->
    case Value of
        I when I > 1114111 ->
            {error, nil};

        65534 ->
            {error, nil};

        65535 ->
            {error, nil};

        I@1 when (I@1 >= 55296) andalso (I@1 =< 57343) ->
            {error, nil};

        I@2 ->
            {ok, gleam_stdlib:identity(I@2)}
    end.
