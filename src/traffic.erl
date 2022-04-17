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
            register(V, spawn(intersection, run, [I]))
    end.

%% (Curried) Takes a graph and a speed, start vertex, and finish
%% vertex.  Spawns a car that travels from start to finish at the
%% given speed.
start_car(G) ->
    fun({Speed, Start, Finish}) ->
            Stops = digraph:get_short_path(G, Start, Finish),
            Car   = car:new(Speed, Stops),
            spawn(car, run, [Car])
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



launch_visualizer() ->
    open_port({spawn, "python3 -u python_listener.py"},
              [binary, {packet, 4}]).

communicate(Port) ->
    receive
        go -> signal_go();
        M  -> io:format("bad message: ~w~n", [M])
    end.


run(Path) ->
    G    = load_graph(Path),
    Cars = load_cars(Path),

    %% launch a process for each intersection,
    %% and label each intersection with corresponding pid
    lists:map(start_intersection(G), digraph:vertices(G)),
    %% start cars
    lists:map(start_car(G), Cars),

    io:format('started everything~n'),

    %% kill switch, for convenience
    %% receive
    %%     kill -> lists:map(fun(V) -> exit(whereis(V), kill) end, 
    %%                       digraph:vertices(G))
    %% end,

    %% launch the python front-end
    Port = launch_visualizer(),
    %% communicate with front-end
    communicate(Port),

    %% the end
    ok.
