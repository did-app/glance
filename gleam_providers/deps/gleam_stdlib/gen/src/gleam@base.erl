-module(gleam@base).
-compile(no_auto_import).

-export([encode64/2, decode64/1, url_encode64/2, url_decode64/1]).

encode64(Input, Padding) ->
    Encoded = base64:encode(Input),
    case Padding of
        true ->
            Encoded;

        false ->
            gleam@string:replace(Encoded, <<"="/utf8>>, <<""/utf8>>)
    end.

decode64(Encoded) ->
    Padded = case gleam@bit_string:byte_size(
        gleam@bit_string:from_string(Encoded)
    )
    rem 4 of
        0 ->
            Encoded;

        N ->
            gleam@string:append(
                Encoded,
                gleam@string:repeat(<<"="/utf8>>, 4 - N)
            )
    end,
    gleam_stdlib:base_decode64(Padded).

url_encode64(Input, Padding) ->
    gleam@string:replace(
        gleam@string:replace(
            encode64(Input, Padding),
            <<"+"/utf8>>,
            <<"-"/utf8>>
        ),
        <<"/"/utf8>>,
        <<"_"/utf8>>
    ).

url_decode64(Encoded) ->
    decode64(
        gleam@string:replace(
            gleam@string:replace(Encoded, <<"-"/utf8>>, <<"+"/utf8>>),
            <<"_"/utf8>>,
            <<"/"/utf8>>
        )
    ).
