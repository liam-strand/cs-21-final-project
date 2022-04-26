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

all: dependencies run

%.beam: ../src/%.erl
	erlc $<

dependencies: python_lib toml_erl_lib

python_lib:
	python3 -m pip -q --exists-action i install --user pygame
	python3 -m pip -q --exists-action i install --user erlastic
	python3 -m pip -q --exists-action i install --user tomli
	python3 -m pip -q --exists-action i install --user grandalf

toml_erl_lib:
	@echo "cd ../lib/toml/src && echo \"yecc:file(toml_parser).\" | erl" | sh
	@echo "cd ../lib/toml/src && echo \"leex:file(toml_lexer).\" | erl" | sh
	@echo "cd ../lib/toml && make install-erlang" | sh

traffic.beam: ports.beam car.beam intersection.beam

run: traffic.beam
	@echo "\n\n* * * * * * * * * * * *\n* Starting" \
	"Simulation *\n* * * * * * * * * * * *\n\n"

	@echo "traffic:run(\"../roads/more_roads.toml\")." | erl -pa ../lib/toml/ebin

%.toml: traffic.beam
	@echo "traffic:run(\"../roads/$@\")." | erl -pa ../lib/toml/ebin

clean:
	rm -f *.beam
	@echo "cd ../lib/toml && make clean" | sh
