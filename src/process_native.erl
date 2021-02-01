-module(process_native).

-export([do_receive/1]).

do_receive(Milliseconds) ->
    receive
        Message -> {ok, Message}
        after Milliseconds -> {error, nil}
    end.
