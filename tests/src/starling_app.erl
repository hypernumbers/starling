-module(starling_app).

-behavior(application).

-export([start/2,
         stop/1]).

start(_Type, _Args) ->
    {ok, ExtProg} = application:get_env(starling, extprog),
    starling_sup:start_link(ExtProg).

stop(_State) ->
    ok.
