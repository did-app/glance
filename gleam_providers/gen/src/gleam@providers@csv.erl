-module(gleam@providers@csv).
-compile(no_auto_import).

-export([extension/0, provide/1]).

extension() ->
    <<".schema.env"/utf8>>.

render(Specs) ->
    gleam@string:replace(
        gleam@string:replace(
            gleam@string:replace(
                <<"
import gleam/map
import gleam/option.{Option}
import gleam/os
import gleam/result

pub type Env{
    Env(
      RECORD_FIELDS
    )
}

pub fn from_env() {
    let raw = os.get_env()
    cast_env(raw)
}

pub fn cast_env(raw) {
    ENV_CHECKS
    Ok(Env(RECORD_KEYS))
}
"/utf8>>,
                <<"RECORD_FIELDS"/utf8>>,
                record_fields(Specs)
            ),
            <<"ENV_CHECKS"/utf8>>,
            env_checks(Specs)
        ),
        <<"RECORD_KEYS"/utf8>>,
        record_keys(Specs)
    ).

record_fields(Specs) ->
    gleam@string:join(
        gleam@list:map(Specs, fun record_field/1),
        <<",\r\n"/utf8>>
    ).

record_field(Spec) ->
    {Key, Spec@1} = Spec,
    case Spec@1 of
        required ->
            gleam@string:concat(
                [gleam@string:lowercase(Key), <<": String"/utf8>>]
            );

        {fallback, _} ->
            gleam@string:concat(
                [gleam@string:lowercase(Key), <<": String"/utf8>>]
            );

        optional ->
            gleam@string:concat(
                [gleam@string:lowercase(Key), <<": Option(String)"/utf8>>]
            )
    end.

env_checks(Specs) ->
    gleam@string:join(gleam@list:map(Specs, fun env_check/1), <<"\r\n"/utf8>>).

env_check(Spec) ->
    case Spec of
        {Key, required} ->
            gleam@string:replace(
                gleam@string:replace(
                    <<"try RECORD_KEY = map.get(raw, \"ENV_KEY\")"/utf8>>,
                    <<"RECORD_KEY"/utf8>>,
                    gleam@string:lowercase(Key)
                ),
                <<"ENV_KEY"/utf8>>,
                Key
            );

        {Key@1, optional} ->
            gleam@string:replace(
                gleam@string:replace(
                    <<"let RECORD_KEY = map.get(raw, \"ENV_KEY\") |> option.from_result"/utf8>>,
                    <<"RECORD_KEY"/utf8>>,
                    gleam@string:lowercase(Key@1)
                ),
                <<"ENV_KEY"/utf8>>,
                Key@1
            );

        {Key@2, {fallback, Fallback}} ->
            gleam@string:replace(
                gleam@string:replace(
                    gleam@string:replace(
                        <<"let RECORD_KEY = map.get(raw, \"ENV_KEY\") |> result.unwrap(\"FALLBACK\")"/utf8>>,
                        <<"RECORD_KEY"/utf8>>,
                        gleam@string:lowercase(Key@2)
                    ),
                    <<"ENV_KEY"/utf8>>,
                    Key@2
                ),
                <<"FALLBACK"/utf8>>,
                Fallback
            )
    end.

record_keys(Specs) ->
    gleam@string:join(
        gleam@list:map(
            Specs,
            fun(Spec) -> gleam@string:lowercase(erlang:element(1, Spec)) end
        ),
        <<", "/utf8>>
    ).

provide(Raw) ->
    render(
        gleam@list:map(
            gleam@list:filter(
                gleam@list:map(
                    gleam@string:split(Raw, <<"\n"/utf8>>),
                    fun gleam@string:trim/1
                ),
                fun non_empty/1
            ),
            fun parse_spec/1
        )
    ).

non_empty(Line) ->
    <<""/utf8>> /= Line.

parse_spec(Line) ->
    case gleam@string:split_once(Line, <<"?"/utf8>>) of
        {ok, {Key, <<""/utf8>>}} ->
            {Key, optional};

        {ok, {Key@1, Fallback}} ->
            {Key@1, {fallback, Fallback}};

        {error, nil} ->
            {Line, required}
    end.
