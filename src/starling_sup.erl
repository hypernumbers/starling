
-module(starling_sup).
-behaviour(supervisor).

-export([start_link/1,
         init/1]).

-define(SERVER, ?MODULE).

%% Starts the supervisor.
start_link(Args) ->
    supervisor:start_link(starling_sup, Args).

%% Supervisor callback. Returns restart strategy, maximum restart frequency,
%% and child specs.
init([ExtProg, PoolSize, Group]) ->
    ChildSpecs = get_childspecs(PoolSize, ExtProg, Group, []),
    {ok, {{one_for_one, 3, 10},
          ChildSpecs}}.

get_childspecs(0, _ExtProg, _Group, Acc) -> Acc;
get_childspecs(N, ExtProg, Group, Acc) ->
    ID = list_to_atom("starling_server_" ++ integer_to_list(N)),
    Child = {ID,{starling_server, start_link,
                 [ExtProg, ID, Group]},
             permanent, 10, worker, [starling_server]},
    NewAcc = [Child | Acc],
    get_childspecs(N - 1, ExtProg, Group, NewAcc).
