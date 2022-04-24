"""
sim_state.py

CS 21 Final Project
on-the-road-again
April 2022

This class provides a representation of the initial state of the simulation.
Honnestly, it is a little bit overkill to have a class just for this, but
classes are fun so lets go for it.

We use tomli to parse the toml file containing the graph data. We use the
internal name tomllib to future-proof the code for when tomli gets integrated
into the standard library under the name tomllib in Python 3.11.

"""

import tomli as tomllib
from pprint import pprint


class Sim_State:
    def __init__(self, config_filename: str) -> None:
        with open(config_filename, "rb") as f:
            config = tomllib.load(f)
        self.intersections = config["intersections"]
        self.roads = config["roads"]
        

    def __repr__(self):
        return f"{{\nintersections: {self.intersections},\nroads:         {self.roads},\n}}"


if __name__ == "__main__":
    test = Sim_State("../roads/roads.toml")
    pprint(test)
