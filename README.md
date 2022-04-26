
README

CS 21 Final Project\
on-the-road-again\
April 2022

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
