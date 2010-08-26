#!/usr/bin/env escript
%%% Find the location of erl_interface.

main(_) ->
    io:format("~s~n", [code:lib_dir(erl_interface)]).
