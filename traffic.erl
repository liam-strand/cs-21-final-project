-module(traffic).
-export([run/0]).


%% deviations from initial design:
%% - car progress is [0, road length], not [0, 1]
%% - car searches for destination while it's waiting,
%%   not after it gets signaled (more realistic)

%% cars wait in queues of (road, next road) => [car]
%% isect cycles thru queue and dequeues n cars at a time
%% wait some number of seconds after dequeueing one road

%% use digraph:get_short_path for pathfinding


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


%% should be spawned, so that the kill switch can be utilized
run() ->
    G = digraph:new(),

    %% add intersections
    digraph:add_vertex(G, alewife),
    digraph:add_vertex(G, davis),
    digraph:add_vertex(G, porter),
    digraph:add_vertex(G, harvard),
    digraph:add_vertex(G, central),
    digraph:add_vertex(G, 'kendall/mit'),
    digraph:add_vertex(G, 'charles/mgh'),
    digraph:add_vertex(G, 'park street'),
    digraph:add_vertex(G, 'downtown crossing'),
    digraph:add_vertex(G, 'south station'),

    %% add roads
    digraph:add_edge(G, alewife, davis),
    digraph:add_edge(G, davis, porter),
    digraph:add_edge(G, porter, harvard),
    digraph:add_edge(G, harvard, central),
    digraph:add_edge(G, central, 'kendall/mit'),
    digraph:add_edge(G, 'kendall/mit', 'charles/mgh'),
    digraph:add_edge(G, 'charles/mgh', 'park street'),
    digraph:add_edge(G, 'park street', 'downtown crossing'),
    digraph:add_edge(G, 'downtown crossing', 'south station'),

    %% launch a process for each intersection,
    %% and label each intersection with corresponding pid
    lists:map(start_intersection(G), digraph:vertices(G)),
    %% start cars
    lists:map(start_car(G), [{0.1, alewife, 'south station'}]),

    io:format('started everything~n'),

    %% kill switch, for convenience
    receive
        kill -> lists:map(fun(V) -> exit(whereis(V), kill) end, 
                          digraph:vertices(G))
    end,

    %% the end
    ok.
    
