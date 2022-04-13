
all: dependencies erlcomp run

%.beam: ../src/%.erl
	erlc $<

dependencies:
	pip install pygame
	pip install erlastic
	pip install tomli

erlcomp: car.beam intersection.beam traffic.beam

run: erlcomp
	@echo "traffic:run(\"../src/roads.toml\")." | erl

clean:
	rm -f *.beam
