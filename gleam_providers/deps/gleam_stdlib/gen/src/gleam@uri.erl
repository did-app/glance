-module(gleam@uri).
-compile(no_auto_import).

-export([erl_parse/1, parse/1, parse_query/1, query_to_string/1, percent_encode/1, percent_decode/1, path_segments/1, to_string/1, origin/1, merge/2]).

erl_parse(A) ->
    uri_string:parse(A).

parse(String) ->
    case gleam@result:nil_error(gleam@dynamic:map(uri_string:parse(String))) of
        {error, Gleam@try_error} -> {error, Gleam@try_error};
        {ok, Uri_map} ->
            Get = fun(K, Decode_type) ->
                gleam@option:from_result(
                    gleam@result:then(
                        gleam@map:get(Uri_map, gleam@dynamic:from(K)),
                        gleam@function:compose(
                            Decode_type,
                            fun gleam@result:nil_error/1
                        )
                    )
                )
            end,
            Uri = {uri,
                   Get(scheme, fun gleam@dynamic:string/1),
                   Get(userinfo, fun gleam@dynamic:string/1),
                   Get(host, fun gleam@dynamic:string/1),
                   Get(port, fun gleam@dynamic:int/1),
                   gleam@option:unwrap(
                       Get(path, fun gleam@dynamic:string/1),
                       <<""/utf8>>
                   ),
                   Get(query, fun gleam@dynamic:string/1),
                   Get(fragment, fun gleam@dynamic:string/1)},
            {ok, Uri}
    end.

parse_query(Query) ->
    Bool_value = fun(X) ->
        gleam@result:map(gleam@dynamic:bool(X), fun(_) -> <<""/utf8>> end)
    end,
    Query_param = fun(Gleam@capture_variable) ->
        gleam@dynamic:typed_tuple2(
            Gleam@capture_variable,
            fun gleam@dynamic:string/1,
            fun(Gleam@capture_variable@1) ->
                gleam@dynamic:any(
                    Gleam@capture_variable@1,
                    [fun gleam@dynamic:string/1, Bool_value]
                )
            end
        )
    end,
    gleam@result:nil_error(
        gleam@dynamic:typed_list(uri_string:dissect_query(Query), Query_param)
    ).

query_to_string(Query) ->
    gleam@result:unwrap(
        gleam@dynamic:string(
            uri_string:compose_query(Query, [{encoding, utf8}])
        ),
        <<""/utf8>>
    ).

percent_encode(Value) ->
    gleam@string:replace(
        query_to_string([{<<"k"/utf8>>, Value}]),
        <<"k="/utf8>>,
        <<""/utf8>>
    ).

percent_decode(Value) ->
    gleam@result:map(
        gleam@result:then(
            parse_query(gleam@string:concat([<<"k="/utf8>>, Value])),
            fun gleam@list:head/1
        ),
        fun gleam@pair:second/1
    ).

do_remove_dot_segments(Input, Accumulator) ->
    case Input of
        [] ->
            gleam@list:reverse(Accumulator);

        [Segment | Rest] ->
            Accumulator@5 = case {Segment, Accumulator} of
                {<<""/utf8>>, Accumulator@1} ->
                    Accumulator@1;

                {<<"."/utf8>>, Accumulator@2} ->
                    Accumulator@2;

                {<<".."/utf8>>, []} ->
                    [];

                {<<".."/utf8>>, [_ | Accumulator@3]} ->
                    Accumulator@3;

                {Segment@1, Accumulator@4} ->
                    [Segment@1 | Accumulator@4]
            end,
            do_remove_dot_segments(Rest, Accumulator@5)
    end.

remove_dot_segments(Input) ->
    do_remove_dot_segments(Input, []).

path_segments(Path) ->
    remove_dot_segments(gleam@string:split(Path, <<"/"/utf8>>)).

to_string(Uri) ->
    Field = fun(Key, Value) -> case Value of
            {some, V} ->
                {ok, {Key, gleam@dynamic:from(V)}};

            none ->
                {error, nil}
        end end,
    gleam@result:unwrap(
        gleam@dynamic:string(
            uri_string:recompose(
                gleam@map:from_list(
                    gleam@list:filter_map(
                        [Field(scheme, erlang:element(2, Uri)),
                         Field(userinfo, erlang:element(3, Uri)),
                         Field(host, erlang:element(4, Uri)),
                         Field(port, erlang:element(5, Uri)),
                         Field(path, {some, erlang:element(6, Uri)}),
                         Field(query, erlang:element(7, Uri)),
                         Field(fragment, erlang:element(8, Uri))],
                        fun(X) -> X end
                    )
                )
            )
        ),
        <<""/utf8>>
    ).

origin(Uri) ->
    {uri, Scheme, _, Host, Port, _, _, _} = Uri,
    case Scheme of
        {some, <<"https"/utf8>>} ->
            Origin = {uri, Scheme, none, Host, Port, <<""/utf8>>, none, none},
            {ok, to_string(Origin)};

        {some, <<"http"/utf8>>} ->
            Origin = {uri, Scheme, none, Host, Port, <<""/utf8>>, none, none},
            {ok, to_string(Origin)};

        _ ->
            {error, nil}
    end.

drop_last(Elements) ->
    gleam@list:take(Elements, gleam@list:length(Elements) - 1).

join_segments(Segments) ->
    gleam@string:join([<<""/utf8>> | Segments], <<"/"/utf8>>).

merge(Base, Relative) ->
    case Base of
        {uri, {some, _}, _, {some, _}, _, _, _, _} ->
            case Relative of
                {uri, _, _, {some, _}, _, _, _, _} ->
                    Path = join_segments(
                        remove_dot_segments(
                            gleam@string:split(
                                erlang:element(6, Relative),
                                <<"/"/utf8>>
                            )
                        )
                    ),
                    Resolved = {uri,
                                gleam@option:'or'(
                                    erlang:element(2, Relative),
                                    erlang:element(2, Base)
                                ),
                                none,
                                erlang:element(4, Relative),
                                erlang:element(5, Relative),
                                Path,
                                erlang:element(7, Relative),
                                erlang:element(8, Relative)},
                    {ok, Resolved};

                {uri, none, _, none, _, _, _, _} ->
                    {New_path, New_query} = case erlang:element(6, Relative) of
                        <<""/utf8>> ->
                            {erlang:element(6, Base),
                             gleam@option:'or'(
                                 erlang:element(7, Relative),
                                 erlang:element(7, Base)
                             )};

                        _ ->
                            Path_segments = case gleam@string:starts_with(
                                erlang:element(6, Relative),
                                <<"/"/utf8>>
                            ) of
                                true ->
                                    gleam@string:split(
                                        erlang:element(6, Relative),
                                        <<"/"/utf8>>
                                    );

                                false ->
                                    gleam@list:append(
                                        drop_last(
                                            gleam@string:split(
                                                erlang:element(6, Base),
                                                <<"/"/utf8>>
                                            )
                                        ),
                                        gleam@string:split(
                                            erlang:element(6, Relative),
                                            <<"/"/utf8>>
                                        )
                                    )
                            end,
                            Path@1 = join_segments(
                                remove_dot_segments(Path_segments)
                            ),
                            {Path@1, erlang:element(7, Relative)}
                    end,
                    Resolved@1 = {uri,
                                  erlang:element(2, Base),
                                  none,
                                  erlang:element(4, Base),
                                  erlang:element(5, Base),
                                  New_path,
                                  New_query,
                                  erlang:element(8, Relative)},
                    {ok, Resolved@1}
            end;

        _ ->
            {error, nil}
    end.
