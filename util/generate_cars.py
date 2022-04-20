from pprint import pp, pprint
import sys
import tomli as tomllib
from random import random, randrange
from pprint import pprint

def main(argv):
    with open(argv[1], "rb") as f:
        config = tomllib.load(f)
    intersections = config["intersections"]

    for inter in intersections:
        for _ in range(6):
            other_inter_idx = randrange(0, len(intersections) - 1)
            inters_copy = list(intersections)
            inters_copy.remove(inter)
            other_inter = inters_copy[other_inter_idx]
            speed = randrange(20, 50) / 1000
            print(f"\n[[cars]]\nspeed = {speed}\nstart = \"{inter}\"\nend   = \"{other_inter}\"")

if __name__ == "__main__":
    main(sys.argv)
