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
start_car(G, Port_Manager) ->
    fun({Speed, Start, Finish}) ->
            Stops = digraph:get_short_path(G, Start, Finish),
            case Stops of
                false -> io:format("*** no path from ~w to ~w.~n",
                                   [Start, Finish]),
                         io:format("*** giving up.~n"),
                         erlang:halt();
                S -> Car = car:new(Speed, S),
                     io:format("~w~n", [Car]),
                     spawn(car, run, [Car, Port_Manager])
            end
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
                      % digraph:add_edge(G, B, A) 
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

%% should be spawned, so that the kill switch can be utilized
run(Path) ->
    G    = load_graph(Path),
    Cars = load_cars(Path),
    io:format("~w~n", [Cars]),
    Port_Manager = spawn(ports, manage, [self(), Path]),
    receive {Port_Manager, ready} -> ok end,

    %% launch a process for each intersection,
    %% and label each intersection with corresponding pid
    lists:map(start_intersection(G), digraph:vertices(G)),
    %% start cars
    lists:map(start_car(G, Port_Manager), Cars),

    io:format('started everything~n'),

    %% kill switch, for convenience
    receive
        kill -> lists:map(fun(V) -> exit(whereis(V), kill) end, 
                          digraph:vertices(G))
    end,

    %% the end
    ok.
    
