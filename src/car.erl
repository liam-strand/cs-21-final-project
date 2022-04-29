%% A representation of a car as a process.  The car travels between a
%% series of intersections, waits at the intersections, and uses a
%% port manager to update a process with its current state.
-module(car).
-export([new/2, run/2]).

%% A car that runs at `speed' and visits intersections in the list
%% `stops' in order.  The intersections are expected to be registered
%% process names, identifying intersection processes.
-record(car, {speed, pos=0, stops}).


%% Create a new car record.
new(Speed, Stops) -> #car{speed=Speed, stops=Stops}.


%% Have a car drive from its start to its destination,
%% waiting at all intersections until signaled.

%% Finish if at end of path.  There are various stopping
%% conditions:
%% (a) The car has less than two stops left to visit;
%% (b) the car is at the end of the road, 
%%     between its last two intersections.
run(#car{stops=[ ]}, Port_Manager) -> 
    Port_Manager ! {self(), finished, none},
    io:format("~w finished!~n", [self()]);
run(#car{stops=[_]}, Port_Manager) -> 
    Port_Manager ! {self(), finished, none},
    io:format("~w finished!~n", [self()]);
run(#car{stops=[_,_],pos=P}, Port_Manager) when P >= 1.0 -> 
    Port_Manager ! {self(), finished, none},
    io:format("~w finished!~n", [self()]);

%% Wait at intersection when we reach it.
run(Car, Port_Manager) when Car#car.pos >= 1.0 -> 
    run(wait(Car, Port_Manager), Port_Manager);

%% Otherwise, just advance along the current road.
run(Car, Port_Manager) -> 
    update(Car, Port_Manager),
    timer:sleep(100),
    run(Car#car{pos = Car#car.pos + Car#car.speed}, Port_Manager).



%% Wait at an intersection, and continue on to the next road when
%% signaled.
wait(#car{stops=[Prev|[Cur|Next]], speed=S}, Port_Manager) -> 
    Cur ! {wait, self(), Prev, hd(Next)},
    update(waiting, Port_Manager, Cur),
    io:format("~w waiting at ~w~n", [self(), Cur]),
    receive
        go -> io:format("~w got signal -> ~w~n", [self(), Next]),
              #car{speed=S, stops=[Cur|Next]}
    end.

%% Send an update on the car's state to the port manager.
update(Car, Port_Manager) ->
    [Prev | [Next |_]] = Car#car.stops,
    Port_Manager ! {self(), update, {Car#car.pos, Prev, Next}}.
%% Send an update that simply says where a car is waiting.
update(waiting, Port_Manager, Cur) ->
    Port_Manager ! {self(), waiting, {Cur}}.
