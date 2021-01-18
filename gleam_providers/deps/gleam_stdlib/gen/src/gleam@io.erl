-module(gleam@io).
-compile(no_auto_import).

-export([print/1, println/1, debug/1]).

print(String) ->
    io:fwrite(String, []),
    nil.

println(String) ->
    io:fwrite(<<"~ts\n"/utf8>>, [String]),
    nil.

debug(Term) ->
    io:fwrite(<<"~tp\n"/utf8>>, [Term]),
    Term.
