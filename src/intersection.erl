%% Defines an abstract interfaces for an intersection, which runs as a
%% process.  The intersection can be sent a message of the form
%% `{wait, Pid, From, To}', which enqueues the process at `Pid', and
%% signals that process with the atom `go' when the path going from
%% intersection `From' to `To' is open.
-module(intersection).
-export([new/2, run/1]).

-record(intersect, {in=[], out=[], queues=#{}, lights}).



%% Repeatedly enqueue and signal cars.
run(__I) -> 
    _I = cycle_lights(__I),
    I  = register_car(_I),
    timer:sleep(1000),
    run(I).


%% Create a new intersection, given incoming and outgoing roads.
new(In, Out) ->
    Lights = [{From, To} || From <- In, To <- Out],
    Queues = lists:map(fun(K) -> {K, queue:new()} end, Lights),
    #intersect{in=In, out=Out, 
               queues=maps:from_list(Queues), 
               lights=queue:from_list(Lights)}.


%% Queue up a car, associating it in the queue map with
%% `{From, To}', its origin and destination.
enqueue_car(Queues, Pid, From, To) ->
    Enqueue = fun(Q) -> queue:in(Pid, Q) end,
    maps:update_with({From, To}, Enqueue, Queues).


%% Listen for a car and enqueue it at a light; quits
%% after a certain amount of time.
register_car(I) ->
    receive
        {wait, Pid, From, To} ->
            Qs = enqueue_car(I#intersect.queues, Pid, From, To),
            I#intersect{queues=Qs}
    after 100 -> I
    end.



%% Signal the next light in the list of lights.
cycle_lights(I) ->
    case queue:out(I#intersect.lights) of
        {{value, L}, _Q} ->
            Q = queue:in(L, _Q),
            signal_light(I, L),
            I#intersect{lights=Q};
        {empty, _} -> I
    end.


%% Signal a few cars waiting at one of the lights.
signal_light(I, L) -> 
    %% signal cars at start of queue to go
    Q = maps:get(L, I#intersect.queues),

    %% Dequeue at most queue:len(Q) 'lucky' cars.
    SafeLen = max(1, queue:len(Q)),
    RandLen = max(rand:uniform(SafeLen), 5),
    {Lucky, Rest} = queue:split(min(RandLen, queue:len(Q)), Q),

    %% wake up all the cars that have been selected
    awaken_queue(Lucky),
    
    %% update queue map
    Queues = maps:update(L, Rest, I#intersect.queues),
    I#intersect{queues=Queues}.


%% Awaken a queue of cars.  It would be better to use queue:fold to do
%% this, but queue:fold was added in version 24 of OTP, and not all
%% systems (e.g. my Debian system) support OTP 24 yet.
awaken_queue(Q) ->
    L = queue:to_list(Q),
    lists:map(fun(Car) -> Car ! go end, L).
