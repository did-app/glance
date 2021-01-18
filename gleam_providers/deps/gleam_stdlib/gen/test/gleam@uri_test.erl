-module(gleam@uri_test).
-compile(no_auto_import).

-export([full_parse_test/0, parse_only_path_test/0, parse_only_host_test/0, error_parsing_uri_test/0, full_uri_to_string_test/0, path_only_uri_to_string_test/0, parse_query_string_test/0, parse_empty_query_string_test/0, parse_query_string_with_empty_test/0, error_parsing_query_test/0, query_to_string_test/0, empty_query_to_string_test/0, percent_encode_test/0, percent_encode_consistency_test/0, percent_decode_test/0, percent_decode_consistency_test/0, parse_segments_test/0, origin_test/0, merge_test/0]).

full_parse_test() ->
    {ok, Parsed} = gleam@uri:parse(
        <<"https://foo:bar@example.com:1234/path?query=true#fragment"/utf8>>
    ),
    gleam@should:equal(erlang:element(2, Parsed), {some, <<"https"/utf8>>}),
    gleam@should:equal(erlang:element(3, Parsed), {some, <<"foo:bar"/utf8>>}),
    gleam@should:equal(
        erlang:element(4, Parsed),
        {some, <<"example.com"/utf8>>}
    ),
    gleam@should:equal(erlang:element(5, Parsed), {some, 1234}),
    gleam@should:equal(erlang:element(6, Parsed), <<"/path"/utf8>>),
    gleam@should:equal(erlang:element(7, Parsed), {some, <<"query=true"/utf8>>}),
    gleam@should:equal(erlang:element(8, Parsed), {some, <<"fragment"/utf8>>}).

parse_only_path_test() ->
    {ok, Parsed} = gleam@uri:parse(<<""/utf8>>),
    gleam@should:equal(erlang:element(2, Parsed), none),
    gleam@should:equal(erlang:element(3, Parsed), none),
    gleam@should:equal(erlang:element(4, Parsed), none),
    gleam@should:equal(erlang:element(5, Parsed), none),
    gleam@should:equal(erlang:element(6, Parsed), <<""/utf8>>),
    gleam@should:equal(erlang:element(7, Parsed), none),
    gleam@should:equal(erlang:element(8, Parsed), none).

parse_only_host_test() ->
    {ok, Parsed} = gleam@uri:parse(<<"//"/utf8>>),
    gleam@should:equal(erlang:element(2, Parsed), none),
    gleam@should:equal(erlang:element(3, Parsed), none),
    gleam@should:equal(erlang:element(4, Parsed), {some, <<""/utf8>>}),
    gleam@should:equal(erlang:element(5, Parsed), none),
    gleam@should:equal(erlang:element(6, Parsed), <<""/utf8>>),
    gleam@should:equal(erlang:element(7, Parsed), none),
    gleam@should:equal(erlang:element(8, Parsed), none).

error_parsing_uri_test() ->
    gleam@should:equal(gleam@uri:parse(<<"::"/utf8>>), {error, nil}).

full_uri_to_string_test() ->
    Test_uri = {uri,
                {some, <<"https"/utf8>>},
                {some, <<"foo:bar"/utf8>>},
                {some, <<"example.com"/utf8>>},
                {some, 1234},
                <<"/path"/utf8>>,
                {some, <<"query=true"/utf8>>},
                {some, <<"fragment"/utf8>>}},
    gleam@should:equal(
        gleam@uri:to_string(Test_uri),
        <<"https://foo:bar@example.com:1234/path?query=true#fragment"/utf8>>
    ).

path_only_uri_to_string_test() ->
    Test_uri = {uri, none, none, none, none, <<"/"/utf8>>, none, none},
    gleam@should:equal(gleam@uri:to_string(Test_uri), <<"/"/utf8>>).

parse_query_string_test() ->
    {ok, Parsed} = gleam@uri:parse_query(<<"foo+bar=1&city=%C3%B6rebro"/utf8>>),
    gleam@should:equal(
        Parsed,
        [{<<"foo bar"/utf8>>, <<"1"/utf8>>},
         {<<"city"/utf8>>, <<"örebro"/utf8>>}]
    ).

parse_empty_query_string_test() ->
    {ok, Parsed} = gleam@uri:parse_query(<<""/utf8>>),
    gleam@should:equal(Parsed, []).

parse_query_string_with_empty_test() ->
    gleam@should:equal(
        gleam@uri:parse_query(<<"present"/utf8>>),
        {ok, [{<<"present"/utf8>>, <<""/utf8>>}]}
    ).

error_parsing_query_test() ->
    gleam@should:equal(gleam@uri:parse_query(<<"%C2"/utf8>>), {error, nil}).

query_to_string_test() ->
    Query_string = gleam@uri:query_to_string(
        [{<<"foo bar"/utf8>>, <<"1"/utf8>>},
         {<<"city"/utf8>>, <<"örebro"/utf8>>}]
    ),
    gleam@should:equal(Query_string, <<"foo+bar=1&city=%C3%B6rebro"/utf8>>).

empty_query_to_string_test() ->
    Query_string = gleam@uri:query_to_string([]),
    gleam@should:equal(Query_string, <<""/utf8>>).

percent_codec_fixtures() ->
    [{<<" "/utf8>>, <<"+"/utf8>>},
     {<<","/utf8>>, <<"%2C"/utf8>>},
     {<<";"/utf8>>, <<"%3B"/utf8>>},
     {<<":"/utf8>>, <<"%3A"/utf8>>},
     {<<"!"/utf8>>, <<"%21"/utf8>>},
     {<<"?"/utf8>>, <<"%3F"/utf8>>},
     {<<"'"/utf8>>, <<"%27"/utf8>>},
     {<<"("/utf8>>, <<"%28"/utf8>>},
     {<<")"/utf8>>, <<"%29"/utf8>>},
     {<<"["/utf8>>, <<"%5B"/utf8>>},
     {<<"@"/utf8>>, <<"%40"/utf8>>},
     {<<"/"/utf8>>, <<"%2F"/utf8>>},
     {<<"\\"/utf8>>, <<"%5C"/utf8>>},
     {<<"&"/utf8>>, <<"%26"/utf8>>},
     {<<"#"/utf8>>, <<"%23"/utf8>>},
     {<<"="/utf8>>, <<"%3D"/utf8>>},
     {<<"~"/utf8>>, <<"%7E"/utf8>>},
     {<<"ñ"/utf8>>, <<"%C3%B1"/utf8>>},
     {<<"-"/utf8>>, <<"-"/utf8>>},
     {<<"_"/utf8>>, <<"_"/utf8>>},
     {<<"."/utf8>>, <<"."/utf8>>},
     {<<"*"/utf8>>, <<"*"/utf8>>},
     {<<"100% great"/utf8>>, <<"100%25+great"/utf8>>}].

percent_encode_test() ->
    gleam@list:map(percent_codec_fixtures(), fun(T) -> {A, B} = T,
            gleam@should:equal(gleam@uri:percent_encode(A), B) end).

percent_encode_consistency_test() ->
    K = <<"foo bar[]"/utf8>>,
    V = <<"ñaña (,:*~)"/utf8>>,
    Query_string = gleam@uri:query_to_string([{K, V}]),
    Encoded_key = gleam@uri:percent_encode(K),
    Encoded_value = gleam@uri:percent_encode(V),
    Manual_query_string = gleam@string:concat(
        [Encoded_key, <<"="/utf8>>, Encoded_value]
    ),
    gleam@should:equal(Query_string, Manual_query_string).

percent_decode_test() ->
    gleam@list:map(percent_codec_fixtures(), fun(T) -> {A, B} = T,
            gleam@should:equal(gleam@uri:percent_decode(B), {ok, A}) end).

percent_decode_consistency_test() ->
    K = <<"foo+bar[]"/utf8>>,
    V = <<"%C3%B6rebro"/utf8>>,
    Query = gleam@string:concat([K, <<"="/utf8>>, V]),
    {ok, Parsed} = gleam@uri:parse_query(Query),
    {ok, Decoded_key} = gleam@uri:percent_decode(K),
    {ok, Decoded_value} = gleam@uri:percent_decode(V),
    gleam@should:equal(Parsed, [{Decoded_key, Decoded_value}]).

parse_segments_test() ->
    gleam@should:equal(gleam@uri:path_segments(<<"/"/utf8>>), []),
    gleam@should:equal(
        gleam@uri:path_segments(<<"/foo/bar"/utf8>>),
        [<<"foo"/utf8>>, <<"bar"/utf8>>]
    ),
    gleam@should:equal(gleam@uri:path_segments(<<"////"/utf8>>), []),
    gleam@should:equal(
        gleam@uri:path_segments(<<"/foo//bar"/utf8>>),
        [<<"foo"/utf8>>, <<"bar"/utf8>>]
    ),
    gleam@should:equal(gleam@uri:path_segments(<<"/."/utf8>>), []),
    gleam@should:equal(
        gleam@uri:path_segments(<<"/.foo"/utf8>>),
        [<<".foo"/utf8>>]
    ),
    gleam@should:equal(
        gleam@uri:path_segments(<<"/../bar"/utf8>>),
        [<<"bar"/utf8>>]
    ),
    gleam@should:equal(
        gleam@uri:path_segments(<<"../bar"/utf8>>),
        [<<"bar"/utf8>>]
    ),
    gleam@should:equal(
        gleam@uri:path_segments(<<"/foo/../bar"/utf8>>),
        [<<"bar"/utf8>>]
    ).

origin_test() ->
    {ok, Parsed} = gleam@uri:parse(<<"http://example.test/path?foo#bar"/utf8>>),
    gleam@should:equal(
        gleam@uri:origin(Parsed),
        {ok, <<"http://example.test"/utf8>>}
    ),
    {ok, Parsed@1} = gleam@uri:parse(<<"http://example.test:8080"/utf8>>),
    gleam@should:equal(
        gleam@uri:origin(Parsed@1),
        {ok, <<"http://example.test:8080"/utf8>>}
    ),
    {ok, Parsed@2} = gleam@uri:parse(<<"https://example.test"/utf8>>),
    gleam@should:equal(
        gleam@uri:origin(Parsed@2),
        {ok, <<"https://example.test"/utf8>>}
    ),
    {ok, Parsed@3} = gleam@uri:parse(<<"http:///path"/utf8>>),
    gleam@should:equal(gleam@uri:origin(Parsed@3), {ok, <<"http://"/utf8>>}),
    {ok, Parsed@4} = gleam@uri:parse(<<"http://"/utf8>>),
    gleam@should:equal(gleam@uri:origin(Parsed@4), {ok, <<"http://"/utf8>>}),
    {ok, Parsed@5} = gleam@uri:parse(<<"/path"/utf8>>),
    gleam@should:equal(gleam@uri:origin(Parsed@5), {error, nil}),
    {ok, Parsed@6} = gleam@uri:parse(<<"file:///dev/null"/utf8>>),
    gleam@should:equal(gleam@uri:origin(Parsed@6), {error, nil}).

merge_test() ->
    {ok, A} = gleam@uri:parse(<<"/relative"/utf8>>),
    {ok, B} = gleam@uri:parse(<<""/utf8>>),
    gleam@should:equal(gleam@uri:merge(A, B), {error, nil}),
    {ok, A@1} = gleam@uri:parse(<<"http://google.com/foo"/utf8>>),
    {ok, B@1} = gleam@uri:parse(<<"http://example.com/baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@1, B@1),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@2} = gleam@uri:parse(<<"http://google.com/foo"/utf8>>),
    {ok, B@2} = gleam@uri:parse(
        <<"http://example.com/.././bar/../../baz"/utf8>>
    ),
    gleam@should:equal(
        gleam@uri:merge(A@2, B@2),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@3} = gleam@uri:parse(<<"http://google.com/foo"/utf8>>),
    {ok, B@3} = gleam@uri:parse(<<"//example.com/baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@3, B@3),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@4} = gleam@uri:parse(<<"http://google.com/foo"/utf8>>),
    {ok, B@4} = gleam@uri:parse(<<"//example.com/.././bar/../../../baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@4, B@4),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@5} = gleam@uri:parse(<<"http://example.com/foo/bar"/utf8>>),
    {ok, B@5} = gleam@uri:parse(<<"/baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@5, B@5),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@6} = gleam@uri:parse(<<"http://example.com/foo/bar"/utf8>>),
    {ok, B@6} = gleam@uri:parse(<<"baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@6, B@6),
        gleam@uri:parse(<<"http://example.com/foo/baz"/utf8>>)
    ),
    {ok, A@7} = gleam@uri:parse(<<"http://example.com/foo/"/utf8>>),
    {ok, B@7} = gleam@uri:parse(<<"baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@7, B@7),
        gleam@uri:parse(<<"http://example.com/foo/baz"/utf8>>)
    ),
    {ok, A@8} = gleam@uri:parse(<<"http://example.com"/utf8>>),
    {ok, B@8} = gleam@uri:parse(<<"baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@8, B@8),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@9} = gleam@uri:parse(<<"http://example.com"/utf8>>),
    {ok, B@9} = gleam@uri:parse(<<"/.././bar/../../../baz"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@9, B@9),
        gleam@uri:parse(<<"http://example.com/baz"/utf8>>)
    ),
    {ok, A@10} = gleam@uri:parse(<<"http://example.com/foo/bar"/utf8>>),
    {ok, B@10} = gleam@uri:parse(<<""/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@10, B@10),
        gleam@uri:parse(<<"http://example.com/foo/bar"/utf8>>)
    ),
    {ok, A@11} = gleam@uri:parse(<<"http://example.com/foo/bar"/utf8>>),
    {ok, B@11} = gleam@uri:parse(<<"#fragment"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@11, B@11),
        gleam@uri:parse(<<"http://example.com/foo/bar#fragment"/utf8>>)
    ),
    {ok, A@12} = gleam@uri:parse(<<"http://example.com/foo/bar"/utf8>>),
    {ok, B@12} = gleam@uri:parse(<<"?query"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@12, B@12),
        gleam@uri:parse(<<"http://example.com/foo/bar?query"/utf8>>)
    ),
    {ok, A@13} = gleam@uri:parse(<<"http://example.com/foo/bar?query1"/utf8>>),
    {ok, B@13} = gleam@uri:parse(<<"?query2"/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@13, B@13),
        gleam@uri:parse(<<"http://example.com/foo/bar?query2"/utf8>>)
    ),
    {ok, A@14} = gleam@uri:parse(<<"http://example.com/foo/bar?query"/utf8>>),
    {ok, B@14} = gleam@uri:parse(<<""/utf8>>),
    gleam@should:equal(
        gleam@uri:merge(A@14, B@14),
        gleam@uri:parse(<<"http://example.com/foo/bar?query"/utf8>>)
    ).
