import sys
from sim_state import Sim_State

from grandalf.graphs import Vertex, Edge, Graph
from grandalf.layouts import DigcoLayout


def embed(sim: Sim_State):
    # Grandalf prints its info to stdout, which will
    # mess up communication with erlang.  Here we
    # trick it into printing to stdout.
    tmp = sys.stdout
    sys.stdout = sys.stderr


    vertices = [Vertex(data) for data in sim.intersections]
    v_dict = {}

    for v in vertices:
        v_dict[v.data] = v

    edges = [Edge(v_dict[r[0]], v_dict[r[1]]) for r in sim.roads]

    graph = Graph(vertices, edges)
    graph.C

    class defaultView(object):
        w, h = 10, 10

    for v in vertices:
        v.view = defaultView()

    dig = DigcoLayout(graph.C[0])

    dig.init_all()

    dig.draw()

    embeding = {}

    for v in graph.C[0].sV:
        embeding[v.data] = (v.view.xy[0], v.view.xy[1])

    sys.stdout = tmp

    return embeding
