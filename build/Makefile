
all: dependencies run

%.beam: ../src/%.erl
	erlc $<

dependencies:
	@echo -e"\n\n* * * * * * * * * * * * *\n* Installing " \
	"Libraries *\n* * * * * * * * * * * * *\n\n"

	python3 -m pip -q --exists-action i install --user pygame
	python3 -m pip -q --exists-action i install --user erlastic
	python3 -m pip -q --exists-action i install --user tomli
	python3 -m pip -q --exists-action i install --user ../grandalf

	@echo "\n\n* * * * * * * * * * *\n* Compiling " \
	"Erlang *\n* * * * * * * * * * *\n\n"

traffic.beam: car.beam intersection.beam 

run: traffic.beam
	@echo "\n\n* * * * * * * * * * * *\n* Starting" \
	"Simulation *\n* * * * * * * * * * * *\n\n"

	@echo "traffic:run(\"../src/roads.toml\")." | erl

clean:
	rm -f *.beam
