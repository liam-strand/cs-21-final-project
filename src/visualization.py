"""
visualization.py

CS 21 Final Project
on-the-road-again
April 2022

This file contains the visualization driver. It is envoked when the
visualization port is opened by the erlang simulation.

"""

from sys import argv

from sim_state import Sim_State
from embed_graph import embed
from display import display_graph


def main(args):
    filename = args[1]

    # Read the roads/intersections from the toml file and perform a 2D
    # embeding
    sim = Sim_State(filename)
    embeding = embed(sim)

    # Spin up the visualization
    display(embeding, sim.roads)


if __name__ == "__main__":
    main(argv)
