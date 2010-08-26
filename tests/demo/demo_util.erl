-module(demo_util).
-export([readlines/1]).

readlines(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    readlines(Device, []).

readlines(Device, Acc) ->
    case io:get_line(Device, "") of
        eof  -> 
            file:close(Device), 
            Acc;
        Line when Line =/= "\n" -> 
            readlines(Device, Acc ++ [string:strip(Line, right, $\n)]);
        _ ->
            readlines(Device, Acc)
    end.
