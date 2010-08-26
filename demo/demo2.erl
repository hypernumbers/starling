-module(demo2).
-export([run/0]).
-import(demo_util, [readlines/1]).

-define(pr(X), ustring:pr(X)).

run() ->
    application:start(starling_app),

    [S1, S2, S3, S4] = lists:map(fun(X) ->
                                         ustring:new(X)
                                 end,
                                 readlines("demo2.txt")),

    Pair1Eql = ustring:eql(S1, S2),
    Pair2Eql = ustring:eql(S3, S4),

    io:format("~s == ~s => ~p~n", [?pr(S1), ?pr(S2), Pair1Eql]),
    io:format("~s == ~s => ~p~n", [?pr(S3), ?pr(S4), Pair2Eql]),

    application:stop(starling_app).
