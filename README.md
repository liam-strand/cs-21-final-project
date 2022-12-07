# on-the-road-again

## Demo
[YouTube](https://youtu.be/OnzkSmFvxiM)

## Dependencies

Two dependencies are necessary to run this simulation:

- Python 3, accessible with the `python3` shell command
- Erlang, accessible with the `erl` shell command

All other dependencies are handled by the Makefile.

## Compiling and Running

The following Makefile rules are provided to automate running the
simulation. They must be run from the `build` directory.

#### `make`

Installs all dependencies, compiles libraries and source code, then runs
the simulation with a medium-size road graph.

#### `make dependencies`

Installs all necessary dependencies for the project, including the python
libraries `grandalf`, `tomli`, `pygame`, and `erlastic`. Furthermore, it
builds the `toml` erlang library using that library's Makefile.

#### `make traffic.beam`

Builds the non-library erlang code that runs the actual simulation.

#### `make run`

Runs the simulation with a moderately sized road graph

#### `make <*.toml>`

Runs the simulation with the roads file provided. The `.toml` file must be
in the `roads` directory.

#### `make clean`

Cleans up all build files. Also cleans up compiled libraries.

## Config format
Clients can specify their own custom road networks using a TOML config
file, and can pass that file to the `make` command to simulate the
road network (see above).


- Intersections are stored in the `intersections` field, which is a
  flat list of strings.
- Roads are stored in `roads`, which is a list of pairs of
  intersection names.  The first name in the pair specifies the source
  intersection, and the second is the destination.
- Individual cars are defined underneath a `[[cars]]` header.  A car
  has a floating-point field called `speed`, and two string fields
  called `start` and `end`.
  
  
See the `roads/` directory for example configuration files.



## Overview of source code
All code can be found in `src/`, with the exception of `build/Makefile`

- `car.erl`
  - an actor representation of a car; moves along roads, and contacts
    intersections along its route and waits to be signaled
  - `traffic.erl` uses this module to launch car processes
- `car.py`
  - a very simple object-oriented representation of a car
  - used by display.py to package together data about cars
- `clock.erl`
  - sends periodic messages to the python visualization
  - used by `display.py` to synchronize with backend during idle time
- `display.py`
  - initializes visualization
  - scales graph embedding to fit nicely on display
  - Handshakes with erlang to confirm connection
  - Listens to updates from erlang and modifies the displayed image to
    match
- `embed_graph.py`
  - Performs graph embedding of the directed graph 
  - exports a dictionary mapping intersections to points
- `intersection.erl`
  - actor representation of an intersection, which rotates through
    queues of waiting cars and signals them
  - `traffic.erl` uses this module to launch intersection processes
- `build/Makefile`
  - rules to verify and install dependencies
  - rules to run the simulation
- `ports.erl`
  - contains tools for communicating with the python visualization
  - used by car, intersection, and traffic modules to send updates to
python `sim_state.py`
    - a bloated monstrosity for passing the initial configuration from
      the toml file to the graph embedding algorithm.
    - a class is not necessary here, but Liam wanted practice
- `traffic.erl`
    - loads a road network from a TOML file and launches
    visualization, roads, and intersections gets run by the client to
    start everything off
- `visualization.py`
  - the visualization “driver”
  - contains the target script that is invoked when the Erlang port is
    opened
