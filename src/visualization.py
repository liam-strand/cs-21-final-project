from pprint import pprint
import sys
from erlastic import port_connection, encode, Atom

from sim_state import Sim_State
from embed_graph import embed
from display import display_graph

from pprint import pprint

def main(argv):
    filename = argv[1]
    sim = Sim_State(filename)
    embeding = embed(sim)

    display_graph(embeding, sim.roads)

if __name__ == "__main__":
    main(sys.argv)
