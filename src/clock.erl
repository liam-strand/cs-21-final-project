%% A simple clock that periodically sends 'tick' updates to another
%% process.  The clock can be stopped by a specified process.
-module(clock).
-export([start/1, stop/1, run/2]).

%% Launch a clock process.
start(Target) -> spawn(clock, run, [self(), Target]).

%% Stop a clock process.
stop(Clock)   -> Clock ! {self(), stop}.

%% Run a clock process that sends `Target' the tuple
%% `{Caller, tick, none}' every 100 ms.  It will halt
%% if it receives the message `{Caller, stop}'.
run(Caller, Target) ->
    receive
        {Caller, stop} -> ok
    after 100 -> 
        Target ! {Caller, tick, none},
        run(Caller, Target)
    end.
