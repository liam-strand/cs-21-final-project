%% Contains utilities for running a "port manager," which is a process
%% that abstracts over communication with a python visualization
%% program.  The port manager can be communicated with through the
%% following protocol:
%
% `kill_port': kills the python process and shuts down the port manager.
%% `{PID, Tag, Data}': sends {PID, Tag, Data} to the python process.
%% `{Port, {data, Bin}}': shuts down if `Bin' encodes the term `stop'.
-module(ports).
-export([manage/2]).


%% Launch a port manager, which runs the python visualization using
%% the TOML file specified by `Path'.  When the port is fully
%% initialized, `Caller' is sent the message `{PID, ready}'.  When the
%% port manager is finished running, `Caller' is sent the atom `kill'.
manage(Caller, Path) ->
    %% open a port driver that talks to a python process
    %%                                -u limits buffering
    SpawnString = "python3 -u ../src/visualization.py " ++ Path,
    Port = open_port({spawn, SpawnString},
                     [binary, {packet,4}, use_stdio]),
    io:format("opened port on ~s~n", [SpawnString]),

    %% wait for an acknowledgement from the python program
    receive
        {Port, {data, Bin}} ->
            case binary_to_term(Bin) of
                go -> io:format("----- running simulation!~n");
                T  -> io:format("!!! unexpected: ~w~n", [T])
            end
    end,

    Caller ! {self(), ready},
    io:format("manager sent ready~n"),
    port_loop(Port),
    Caller ! kill.


%% Receive messages.  See module-level documentation for an overview
%% of the communication protocol that the port manager uses.
port_loop(Port) -> 
    receive
        kill_port -> 
            Port ! {self(), close},
            ok;
        {Port, {data, Bin}} ->
            case binary_to_term(Bin) of
                stop -> ok
            end;
        {PID, Tag, Data} -> 
            Port ! {self(), {command, term_to_binary({pid_to_list(PID), 
                                                      Tag, Data})}},
            port_loop(Port);
        Other ->
            io:format("unexpected message: ~w~n", [Other])
    end.
