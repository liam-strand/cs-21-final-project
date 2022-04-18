-module(traffic).
-export([run/1]).


%% (Curried) Takes a graph and a vertex (representing an intersection)
%% and spawns an intersection process, whose name is the same as that
%% of the vertex.
start_intersection(G) ->
    fun(V) ->
            In  = digraph:in_neighbours(G, V),
            Out = digraph:out_neighbours(G, V),
            I = intersection:new(In, Out),
            register(V, spawn(intersection, run, [I])),
            V
    end.

%% (Curried) Takes a graph and a speed, start vertex, and finish
%% vertex.  Spawns a car that travels from start to finish at the
%% given speed.
start_car(G) ->
    fun({Speed, Start, Finish}) ->
            Stops = digraph:get_short_path(G, Start, Finish),
            Car   = car:new(Speed, Stops),
            spawn(car, run, [self(), Car])
    end.

load_graph(Path) ->
    %% load info from config file
    {ok, Cfg} = toml:read_file(Path),
    {array,{string,Inters}} = toml:get_value([], "intersections", Cfg),
    {array,{array,Roads}}  = toml:get_value([], "roads", Cfg),
    %% set up the graph
    G = digraph:new(),
    lists:map(fun(I) -> 
                      A = list_to_atom(I),
                      digraph:add_vertex(G, A) 
              end, Inters),
    lists:map(fun(R) -> 
                      {string,[I,J]} = R,
                      A = list_to_atom(I),
                      B = list_to_atom(J),
                      digraph:add_edge(G, A, B) 
              end, Roads),
    G.

load_cars(Path) ->
    {ok, Cfg} = toml:read_file(Path),
    {array, {object, RawCars}} = toml:get_value([], "cars", Cfg),
    
    lists:map(fun(C) ->
        [{<<"speed">>, Speed},
        {<<"end">>, End},
        {<<"start">>, Start}] = C,

        {
            Speed, 
            list_to_atom(binary_to_list(Start)), 
            list_to_atom(binary_to_list(End))
        }

    end, RawCars).


awaken(Pid) -> Pid ! go.


%% collect N updates sent by cars
get_updates(0) -> [];
get_updates(N) -> 
    receive
        X -> [X|get_updates(N - 1)]
    end.


run_mux(Port, Inters, Cars, N) ->
    Updates = get_updates(N),
    Port ! {self(), {command, term_to_binary({update, Updates})}},
    receive
        {Port, {data, Bin}} -> 
            case binary_to_term(Bin) of
                go -> lists:map(fun awaken/1, Cars),
                      lists:map(fun awaken/1, Inters),
                      run_mux(Port, Inters, Cars, N);
                stop -> done
            end
    end.



run(Path) ->
    G    = load_graph(Path),
    Cars = load_cars(Path),

    %% launch a process for each intersection,
    %% and label each intersection with corresponding pid
    Is = lists:map(start_intersection(G), digraph:vertices(G)),
    %% start cars
    Cs = lists:map(start_car(G), Cars),

    io:format('started everything~n'),
    Port = open_port({spawn, "python3 -u python_listener.py"},
                     [binary,{packet,4}]),

    run_mux(Port, Is, Cs, length(Cs)),

    %% the end
    ok.
