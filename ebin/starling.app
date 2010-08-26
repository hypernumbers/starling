{application, starling, [
    {description, "Starling - Unicode strings for Erlang"},
    {vsn, "0.0.1"},
    {modules, [starling_app, starling_server, starling_sup, ustring]},
    {registered, [starling]},
    {applications, [kernel, stdlib]},
    {mod, {starling_app, []}},
    {env, [{extprog, "starling_drv"}, {timeout, 3000}, 
           {poolsize, 5}, {group, starling_pg2}]}
]}.
