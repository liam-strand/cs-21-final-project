
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

	@echo "observer:start(). \ntraffic:run(\"../src/more_roads.toml\")." | erl

clean:
	rm -f *.beam
