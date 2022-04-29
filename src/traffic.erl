%% The top-level module for running a traffic simulation.  To start it
%% up, call `run(Path)', where `Path' is the location of a TOML
%% configuration file that specifies the layout of a road network, as
%% well as cars with speed and destination attributes.  For more
%% information on configuration syntax, see the README.
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
            %% make sure a path from `Start' to `Finish' exists.
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


%% Read a TOML config file specifying a road network as a digraph.
%% Returns a digraph (from the `digraph' module) representing the
%% network.  Edges represent roads, and vertices represent
%% intersections.
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


%% Load car information from the TOML config file at `Path'.
%% Returns car data in the format `{Speed, Start, End}'.
load_cars(Path) ->
    {ok, Cfg} = toml:read_file(Path),
    {array, {object, RawCars}} = toml:get_value([], "cars", Cfg),
    
    lists:map(fun(C) ->
        [{<<"speed">>, Speed},
        {<<"end">>, End},
        {<<"start">>, Start}] = C,

        {
            Speed / 3, 
            list_to_atom(binary_to_list(Start)), 
            list_to_atom(binary_to_list(End))
        }
    end, RawCars).




%% Run a traffic simulation, loading the road network from the TOML
%% file identified by `Path'.  Shuts down when sent the message `kill'.
run(Path) ->
    %% Load digraph and cars from TOML file.
    G    = load_graph(Path),
    Cars = load_cars(Path),
    io:format("~w~n", [Cars]),

    %% Open the port and wait for confirmation of success.
    Port_Manager = spawn(ports, manage, [self(), Path]),
    receive {Port_Manager, ready} -> ok end,

    %% launch a process for each intersection
    lists:map(start_intersection(G), digraph:vertices(G)),
    %% start cars
    lists:map(start_car(G, Port_Manager), Cars),
    %% start the clock
    Clock = clock:start(Port_Manager),

    io:format('started everything~n'),

    %% kill switch, for convenience
    receive
        kill -> 
            lists:map(fun(V) -> exit(whereis(V), kill) end, 
                          digraph:vertices(G)),
            clock:stop(Clock)
    end,

    %% the end
    ok.
    
