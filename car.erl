-module(car).
-export([new/2, run/1]).

-record(car, {speed, pos=0, stops}).


new(Speed, Stops) ->
    #car{speed=Speed, stops=Stops}.


%% Have a car drive from its start intersection to its destination,
%% abiding by all the traffic laws.
%% 
%% Finish if at end of path.
run(#car{stops=[ ]}) -> 
    io:format("~w finished!~n", [self()]);
run(#car{stops=[_]}) -> 
    io:format("~w finished!~n", [self()]);
run(#car{stops=[_,_],pos=P}) when P >= 1.0 -> 
    io:format("~w finished!~n", [self()]);

%% wait at intersection when we reach it
run(Car) when Car#car.pos >= 1.0 -> 
    run(wait(Car));

%% otherwise just go along road
run(Car) -> 
    timer:sleep(500),
    run(Car#car{pos = Car#car.pos + Car#car.speed}).




%% Wait at an intersection, and continue on to the
%% next road when signaled.
wait(#car{stops=[Prev|[Cur|Next]], speed=S}) -> 
    Cur ! {wait, self(), Prev, hd(Next)},
    io:format("~w waiting at ~w~n", [self(), Cur]),
    receive
        go -> io:format("~w got signal -> ~w~n", [self(), Next]),
              #car{speed=S, stops=[Cur|Next]}
    end.
