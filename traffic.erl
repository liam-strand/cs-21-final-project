-module(traffic).
-export([run/0]).

-record(road, {src=nothing, dest=nothing, len=0}).
-record(intersect, {in=[], out=[]}).
-record(car, {speed=0, road, pos=0}).


run_isect(I) -> 
    receive
        {wait, Pid, _} -> 
            [Next|_] = I#intersect.out,
            timer:sleep(5000),
            io:format("~w can go now~n", [Pid]),
            Pid ! {go, Next};
        Msg -> io:format("[isect] bad message: ~w~n", [Msg])
    end.

run_car(Car, Rs, Is) when 
      Car#car.pos >= (Car#car.road)#road.len ->
    io:format("stop!~n"),
    case (Car#car.road)#road.dest of
        nothing -> io:format("reached destination!!!~n");
        Isect -> 
            Isect ! {wait, self(), foo},
            io:format("waiting ... ~n"),
            receive
                {go, R} -> 
                    NewRoad = maps:get(R, Rs),
                    io:format("switched to road '~w' (~w)~n", [R, NewRoad]),
                    run_car(Car#car{road=NewRoad,pos=0}, Rs, Is);
                Msg -> io:format("[car] bad message: ~w~n", [Msg])
            end
        end;
run_car(Car, Rs, Is) -> 
    NewPos = Car#car.pos + Car#car.speed,
    timer:sleep(1000),
    io:format("pos: ~w~n", [NewPos]),
    run_car(Car#car{pos=NewPos}, Rs, Is).


run() ->
    Central  = #intersect{in=['main street'], out=['broadway']},
    Main     = #road{dest='central', len=5},
    Broadway = #road{src='central',  len=10},

    Rs = #{'main street' => Main, broadway => Broadway},
    Is = #{central => Central},                 % may be extraneous

    register(central, spawn(fun() -> run_isect(Central) end)),
    spawn(fun() -> run_car(#car{speed=1, road=Main}, Rs, Is) end),

    io:format("done.~n").
