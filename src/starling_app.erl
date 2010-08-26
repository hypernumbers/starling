-module(starling_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_Type, _Args) ->
    {ok, ExtProg} = application:get_env(starling, extprog),
    {ok, PoolSize} = application:get_env(starling, poolsize),
    {ok, Group} = application:get_env(starling, group),
    pg2:create(Group),
    starling_sup:start_link([ExtProg, PoolSize, Group]).

stop(_State) ->
    ok.
