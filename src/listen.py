import sys
# sys.stdout = sys.stderr


import os
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = "hide"


from erlastic import port_connection, encode, Atom as A

from pprint import pprint

from sim_state import Sim_State
from embed_graph import embed
from display import display_graph
import pygame

from car import Car




def main():
    # Messages go into mailbox, can send encoded terms to port
    mailbox, port = port_connection()
    # equivalent of recieve loop in erl
    for msg in mailbox:
        print("::: got message:", msg, file=sys.stderr)
        port.send(A("go"))
        print("::: sent `go'", file=sys.stderr)




if __name__ == "__main__":
    main()
