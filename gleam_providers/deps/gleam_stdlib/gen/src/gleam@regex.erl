-module(gleam@regex).
-compile(no_auto_import).

-export([compile/2, from_string/1, check/2, split/2, scan/2]).

compile(A, B) ->
    gleam_stdlib:compile_regex(A, B).

from_string(Pattern) ->
    gleam_stdlib:compile_regex(Pattern, {options, false, false}).

check(A, B) ->
    gleam_stdlib:regex_match(A, B).

split(A, B) ->
    gleam_stdlib:regex_split(A, B).

scan(A, B) ->
    gleam_stdlib:regex_scan(A, B).
