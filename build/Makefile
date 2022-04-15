
all: dependencies run

%.beam: ../src/%.erl
	erlc $<

dependencies:
	@echo -e"\n\n* * * * * * * * * * * * *\n* Installing " \
	"Libraries *\n* * * * * * * * * * * * *\n\n"

	python3 -m pip -qu --exists-action i install pygame
	python3 -m pip -qu --exists-action i install erlastic
	python3 -m pip -qu --exists-action i install tomli
	python3 -m pip -qu --exists-action i install ../grandalf

	@echo "\n\n* * * * * * * * * * *\n* Compiling " \
	"Erlang *\n* * * * * * * * * * *\n\n"

traffic.beam: car.beam intersection.beam 

run: traffic.beam
	@echo "\n\n* * * * * * * * * * * *\n* Starting" \
	"Simulation *\n* * * * * * * * * * * *\n\n"

	@echo "traffic:run(\"../src/roads.toml\")." | erl

clean:
	rm -f *.beam
