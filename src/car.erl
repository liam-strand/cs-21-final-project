-module(car).
-export([new/2, run/2]).

-record(car, {speed, pos=0, stops}).


new(Speed, Stops) ->
    io:format("~w~n", [Stops]),
    #car{speed=Speed, stops=Stops}.


%% Have a car drive from its start intersection to its destination,
%% abiding by all the traffic laws.
%% 
%% Finish if at end of path.
run(#car{stops=[ ]}, Port_Manager) -> 
    Port_Manager ! {self(), finished, none},
    io:format("~w finished!~n", [self()]);
run(#car{stops=[_]}, Port_Manager) -> 
    Port_Manager ! {self(), finished, none},
    io:format("~w finished!~n", [self()]);
run(#car{stops=[_,_],pos=P}, Port_Manager) when P >= 1.0 -> 
    Port_Manager ! {self(), finished, none},
    io:format("~w finished!~n", [self()]);

%% wait at intersection when we reach it
run(Car, Port_Manager) when Car#car.pos >= 1.0 -> 
    %% update(Car, Port_Manager, wait),
    run(wait(Car, Port_Manager), Port_Manager);

%% otherwise just go along road
run(Car, Port_Manager) -> 
    update(Car, Port_Manager),
    timer:sleep(100),
    run(Car#car{pos = Car#car.pos + Car#car.speed}, Port_Manager).




%% Wait at an intersection, and continue on to the
%% next road when signaled.
wait(#car{stops=[Prev|[Cur|Next]], speed=S}, Port_Manager) -> 
    Cur ! {wait, self(), Prev, hd(Next)},
    update(waiting, Port_Manager, Cur),
    io:format("~w waiting at ~w~n", [self(), Cur]),
    receive
        go -> io:format("~w got signal -> ~w~n", [self(), Next]),
              #car{speed=S, stops=[Cur|Next]}
    end.

update(Car, Port_Manager) ->
    [Prev | [Next |_]] = Car#car.stops,
    Port_Manager ! {self(), update, {Car#car.pos, Prev, Next}}.

update(waiting, Port_Manager, Cur) ->
    Port_Manager ! {self(), waiting, {Cur}}.
