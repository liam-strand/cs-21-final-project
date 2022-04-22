-module(car).
-export([new/2, run/2]).

-record(car, {speed, pos=0, stops}).


new(Speed, Stops) ->
    io:format("~w~n", [Stops]),
    #car{speed=Speed, stops=Stops}.


run(Mux, Car) ->
    C = update(Car),
    send_state(Mux, C),
    receive
        go    -> run(Mux, C);
        stop  -> ok
    end.

%% send a car state to the multiplexer
send_state(Mux, Car) ->
    Progress = Car#car.pos,
    Road = 
        case Car#car.stops of
            [From|[To|_]] -> {From, To};
            _ -> finished
        end,
    Mux ! {self(), Road, Progress}.


%% Have a car drive from its start intersection to its destination,
%% abiding by all the traffic laws.
%% 
%% Finish if at end of path.
update(#car{stops=[ ]}) -> 
    io:format("~w finished!~n", [self()]);
update(#car{stops=[_]}) -> 
    io:format("~w finished!~n", [self()]);
update(#car{stops=[_,_],pos=P}) when P >= 1.0 -> 
    io:format("~w finished!~n", [self()]);

%% wait at intersection when we reach it
update(Car) when Car#car.pos >= 1.0 -> 
    wait(Car),
    Car;

%% otherwise just go along road
update(Car) -> 
    timer:sleep(500),
    Car#car{pos = Car#car.pos + Car#car.speed}.




%% Wait at an intersection, and continue on to the
%% next road when signaled.
wait(#car{stops=[Prev|[Cur|Next]], speed=S}) -> 
    Cur ! {wait, self(), Prev, hd(Next)},
    io:format("~w waiting at ~w~n", [self(), Cur]),
    receive
        go -> io:format("~w got signal -> ~w~n", [self(), Next]),
              #car{speed=S, stops=[Cur|Next]}
    end.
