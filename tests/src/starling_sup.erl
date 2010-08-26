
-module(starling_sup).
-behaviour(supervisor).

-export([start_link/1,
         init/1]).

-define(SERVER, ?MODULE).

%% Starts the supervisor.
start_link(ExtProg) ->
    supervisor:start_link(starling_sup, ExtProg).

%% Supervisor callback. Returns restart strategy, maximum restart frequency,
%% and child specs.
init(ExtProg) ->
    Child = {starling_server,{starling_server, start_link, [ExtProg]},
             permanent, 10, worker, [starling_server]},

    {ok, {{one_for_one, 3, 10},
          [Child]}}.
