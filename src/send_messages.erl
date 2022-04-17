-module(send_messages).
-export([go/0, stop/0]).



car(Mux) ->
    io:format("car is going~n"),
    N = rand:uniform(),
    timer:sleep(round(N * 1000)),
    Mux ! N,
    receive
        go -> car(Mux)
    end.

%% launch N cars who communicate with `Mux'
launch_cars(Mux, 0) -> [];
launch_cars(Mux, N) ->
    Pid = spawn(fun() -> car(Mux) end),
    [Pid|launch_cars(Mux, N - 1)].


%% collect N updates sent by cars
get_updates(0) -> [];
get_updates(N) -> 
    receive
        X -> [X|get_updates(N - 1)]
    end.


%% send a 'go' message to a list of cars.
awaken_cars(Cars) ->
    Awaken = fun(C) -> C ! go end,
    lists:map(Awaken, Cars).


%% run a multiplexer in a loop, with N cars, communicating with a
%% python program running on `Port'
run_mux(Port, Cars, N) ->
    Updates = get_updates(N),
    Port ! {self(), {command, term_to_binary({update, Updates})}},
    receive
        {Port, {data, Bin}} -> 
            case binary_to_term(Bin) of
                go -> awaken_cars(Cars),
                      io:format("woke cars up~n"),
                      run_mux(Port, Cars, N);
                stop -> done
            end
    end.


go() ->
    %%                          -u limits buffering             encoding spec
    Port = open_port({spawn, "python3 -u python_listener.py"},[binary,{packet,4}]),
    Cars = launch_cars(self(), 5),
    run_mux(Port, Cars, length(Cars)).



stop() -> erlang:halt().
