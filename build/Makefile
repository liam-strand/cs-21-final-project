
all: dependencies run

%.beam: ../src/%.erl
	erlc $<

dependencies:
	@echo "\n\n* * * * * * * * * * * * *\n* Installing " \
	"Libraries *\n* * * * * * * * * * * * *\n\n"
	
	python3 -m pip -q --exists-action i install pygame
	python3 -m pip -q --exists-action i install erlastic
	python3 -m pip -q --exists-action i install tomli
	python3 -m pip -q --exists-action i install ../grandalf

traffic.beam: car.beam intersection.beam 

run: traffic.beam
	@echo "\n\n* * * * * * * * * * * *\n* Starting" \
	"Simulation *\n* * * * * * * * * * * *\n\n"

	@echo "traffic:run(\"../src/roads.toml\")." | erl

clean:
	rm -f *.beam
