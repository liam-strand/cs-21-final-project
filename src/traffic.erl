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


%% should be spawned, so that the kill switch can be utilized
run(Path) ->
    G = load_graph(Path),

    %% launch a process for each intersection,
    %% and label each intersection with corresponding pid
    lists:map(start_intersection(G), digraph:vertices(G)),
    %% start cars
    lists:map(start_car(G), [{0.1, davis, harvard}]),

    io:format('started everything~n'),

    %% kill switch, for convenience
    receive
        kill -> lists:map(fun(V) -> exit(whereis(V), kill) end, 
                          digraph:vertices(G))
    end,

    %% the end
    ok.
    
