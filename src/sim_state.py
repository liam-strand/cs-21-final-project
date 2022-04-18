import tomli as tomllib
from car import Car
from pprint import pprint


class Sim_State:
    def __init__(self, config_filename: str) -> None:
        with open(config_filename, "rb") as f:
            config = tomllib.load(f)
        self.intersections = config["intersections"]
        self.roads = config["roads"]
        self.cars = [
            Car(start=c["start"], end=c["end"], speed=c["speed"])
            for c in config["cars"]
        ]

    def __repr__(self):
        return f"{{\nintersections: {self.intersections},\nroads:         {self.roads},\ncars:          {self.cars}\n}}"


if __name__ == "__main__":
    test = Sim_State("roads.toml")
    pprint(test)
