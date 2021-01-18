-module(gleam@regex_test).
-compile(no_auto_import).

-export([from_string_test/0, compile_test/0, check_test/0, split_test/0, scan_test/0]).

from_string_test() ->
    {ok, Re} = gleam@regex:from_string(<<"[0-9]"/utf8>>),
    gleam@should:equal(gleam@regex:check(Re, <<"abc123"/utf8>>), true),
    gleam@should:equal(gleam@regex:check(Re, <<"abcxyz"/utf8>>), false),
    {error, From_string_err} = gleam@regex:from_string(<<"[0-9"/utf8>>),
    gleam@should:equal(
        From_string_err,
        {compile_error, <<"missing terminating ] for character class"/utf8>>, 4}
    ).

compile_test() ->
    Options = {options, true, false},
    {ok, Re} = gleam@regex:compile(<<"[A-B]"/utf8>>, Options),
    gleam@should:equal(gleam@regex:check(Re, <<"abc123"/utf8>>), true),
    Options@1 = {options, false, true},
    {ok, Re@1} = gleam@regex:compile(<<"^[0-9]"/utf8>>, Options@1),
    gleam@should:equal(gleam@regex:check(Re@1, <<"abc\n123"/utf8>>), true).

check_test() ->
    {ok, Re} = gleam@regex:from_string(<<"^f.o.?"/utf8>>),
    gleam@should:equal(gleam@regex:check(Re, <<"foo"/utf8>>), true),
    gleam@should:equal(gleam@regex:check(Re, <<"boo"/utf8>>), false).

split_test() ->
    {ok, Re} = gleam@regex:from_string(<<" *, *"/utf8>>),
    gleam@should:equal(
        gleam@regex:split(Re, <<"foo,32, 4, 9  ,0"/utf8>>),
        [<<"foo"/utf8>>,
         <<"32"/utf8>>,
         <<"4"/utf8>>,
         <<"9"/utf8>>,
         <<"0"/utf8>>]
    ).

scan_test() ->
    {ok, Re} = gleam@regex:from_string(<<"Gl\\w+"/utf8>>),
    gleam@should:equal(
        gleam@regex:scan(Re, <<"!Gleam"/utf8>>),
        [{match, <<"Gleam"/utf8>>, 1, []}]
    ),
    gleam@should:equal(
        gleam@regex:scan(Re, <<"हGleam"/utf8>>),
        [{match, <<"Gleam"/utf8>>, 3, []}]
    ),
    gleam@should:equal(
        gleam@regex:scan(Re, <<"𐍈Gleam"/utf8>>),
        [{match, <<"Gleam"/utf8>>, 4, []}]
    ),
    {ok, Re@1} = gleam@regex:from_string(<<"[oi]n a(.?) (\\w+)"/utf8>>),
    gleam@should:equal(
        gleam@regex:scan(Re@1, <<"I am on a boat in a lake."/utf8>>),
        [{match, <<"on a boat"/utf8>>, 5, [none, {some, <<"boat"/utf8>>}]},
         {match, <<"in a lake"/utf8>>, 15, [none, {some, <<"lake"/utf8>>}]}]
    ).
