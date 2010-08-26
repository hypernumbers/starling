-module(starling_server).
-behaviour(gen_server).

-record(state, {port}).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

start_link(ExtProg) ->
    Path = code:priv_dir(starling),
    DirName = filename:dirname(Path),
    ExtProg2 = case os:type() of
                   {win32, nt} -> ExtProg ++ ".exe";
                   _ -> ExtProg
               end,
    DrvPath = filename:join(["/", DirName, "ebin", ExtProg2]),
    gen_server:start_link({local, ?MODULE}, starling_server, DrvPath, []).

init(DrvPath) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, DrvPath}, [{packet, 2}, binary, exit_status]),
    {ok, #state{port = Port}}.

handle_call(Msg, _From, #state{port = Port} = State) ->
    erlang:port_command(Port, term_to_binary(Msg)),
    receive
        {Port, {data, Data}} ->
            {reply, binary_to_term(Data), State}
    after ustring:timeout() ->
            {stop, port_timeout, State}
    end.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
