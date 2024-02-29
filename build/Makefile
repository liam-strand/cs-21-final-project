# Makefile
# 
# CS 21 Final Project
# on-the-road-again
# April 2022
# 
# 
# A Makefile providing rules to install dependencies and run the simulation
# 
# Use cases:
# 
# install dependencies:              make dependencies
# run simulation with default file:  make run
# run simulation with specific file: make *.toml
# clean up compiled files:           make clean
# 



# A catch-all rule so that the simulation "just works" when you call make
all: dependencies run

# Get any .beam from its associated .erl
%.beam: ../src/%.erl
	erlc $<

# Install all dependencies
dependencies: python_lib toml_erl_lib

# Python libraries are installed to the user, and ignored if already present
python_lib:
	python3 -m pip -q --exists-action i install --user pygame
	python3 -m pip -q --exists-action i install --user grandalf
	python3 -m pip -q --exists-action i install --user erpy

# The necessary files are compiled for the erlang toml library
toml_erl_lib:
	@echo "cd ../lib/toml/src && echo \"yecc:file(toml_parser).\" | erl" | sh
	@echo "cd ../lib/toml/src && echo \"leex:file(toml_lexer).\" | erl" | sh
	@echo "cd ../lib/toml && make install-erlang" | sh

# traffic.beam relies on these other files so it makes sense to have them
# listed as dependencies here
traffic.beam: ports.beam car.beam intersection.beam clock.beam

# A catch-all that makes it so that running make run runs the simulation in a 
# reasonable way, assuming make dependencies has already been run
run: traffic.beam more_roads.toml

# Run the simulation with any .toml roads file.
%.toml: traffic.beam
	@echo -e "\n\n* * * * * * * * * * * *\n* Starting" \
		"Simulation *\n* * * * * * * * * * * *\n\n"

	@echo "traffic:run(\"../roads/$@\")." | erl -pa ../lib/toml/ebin

# Clean up everything, and call make clean in the erlang toml library
clean:
	rm -f *.beam
	@echo "cd ../lib/toml && make clean" | sh
