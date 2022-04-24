"""
embed_graph.py

CS 21 Final Project
on-the-road-again
April 2022

This file exports just one function, a function that performs the graph
embeding. Since grandalf decided that it wanted to print stuff to stdout
during the embeding, we must redirect everything sent to stdout to stderr to
avoid messing up the port, which uses stdin and stdout to communicate.

"""

import sys
from sim_state import Sim_State

from grandalf.graphs import Vertex, Edge, Graph
from grandalf.layouts import DigcoLayout

def embed(sim: Sim_State):
    # Redirect grandalf's output to stderr, to avoid
    # messing up communication with erlang (argh! evil grandalf!)
    tmp = sys.stdout
    sys.stdout = sys.stderr

    # Extract vertices and edges as grandalf Vertices and Edges
    vertices = [Vertex(data) for data in sim.intersections]
    v_dict = {}

    for v in vertices:
        v_dict[v.data] = v

    edges = [Edge(v_dict[r[0]], v_dict[r[1]]) for r in sim.roads]

    # Instantiate the graph
    graph = Graph(vertices, edges)
    graph.C

    # For whatever reason we need the "desired dimensions" to be encoded
    # in this class...grandalf be grandalf
    class defaultView(object):
        w, h = 100, 100

    for v in vertices:
        v.view = defaultView()

    # Here we actually perform the 2D graph embeding
    dig = DigcoLayout(graph.C[0])
    dig.init_all()
    dig.draw()

    # Then we record the result of that embeding in a dictionary.
    embeding = {}

    for v in graph.C[0].sV:
        embeding[v.data] = (v.view.xy[0], v.view.xy[1])

    # Undo that evil stream redirection from earlier
    sys.stdout = tmp

    return embeding
