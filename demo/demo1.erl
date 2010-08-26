-module(demo1).
-export([run/0]).
-import(demo_util, [readlines/1]).

-define(pr(X), ustring:pr(X)).

run() ->
    application:start(starling_app),

    lists:foreach(fun(X) ->
                          Ustr = ustring:new(X),
                          Upper = ustring:to_upper(Ustr),
                          Lower = ustring:to_lower(Ustr),
                          io:format("~s~n", [?pr(Upper)]),
                          io:format("~s~n", [?pr(Lower)])
                  end,
                  readlines("demo1.txt")),

    application:stop(starling_app).
