
all: dependencies run

%.beam: ../src/%.erl
	erlc $<

dependencies:
	python3 -m pip install pygame
	python3 -m pip install erlastic
	python3 -m pip install tomli

traffic.beam: car.beam intersection.beam 

run: traffic.beam
	@echo "traffic:run(\"../src/roads.toml\")." | erl

clean:
	rm -f *.beam
