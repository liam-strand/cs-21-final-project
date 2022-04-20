-module(ports).
-export([manage/2]).

manage(Caller, Path) ->
    %                                 -u limits buffering
    SpawnString = "python3 -u ../src/visualization.py " ++ Path,
    Port = open_port({spawn, SpawnString},
                     [binary, {packet,4}, use_stdio]),
    io:format("opened port on ~s~n", [SpawnString]),

    Caller ! {self(), ready},
    io:format("manager sent ready~n"),
    
    % print_mailbox(),
    port_loop(Port),

    Caller ! {self(), done}.

port_loop(Port) -> 
    receive
        kill_port -> 
            % io:format("recieved kill message~n"),
            Port ! {self(), close},
            ok;
        {PID, Tag, Data} -> 
            % io:format("recieved message~n"),
            Port ! {self(), {command, term_to_binary({pid_to_list(PID), Tag, Data})}},
            % io:format("sent message~n"),
            port_loop(Port);
        Other ->
            io:format("unexpected message~w~n", [Other])
    end.
