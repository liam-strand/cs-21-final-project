-module(clock).
-export([start/1, stop/1, run/2]).

start(Target) -> spawn(clock, run, [self(), Target]).
stop(Clock)   -> Clock ! {self(), stop}.

run(Caller, Target) ->
    receive
        {Caller, stop} -> ok
    after 100 -> 
        Target ! {Caller, tick, none},
        run(Caller, Target)
    end.
