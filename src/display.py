# IMPORTANT NOTE:
#
# Everything from stdout gets sent to the Erlang program that spawned
# this python process, as part of the port driver abstraction.  This
# means that any erroneous output will confuse the Erlang server and
# effectively break the whole program.  Normally, pygame prints a
# message to stdout when you import it, so we have to set this
# environment variable in order to suppress that message.
import os
os.environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import pygame
import sys
from pprint import pprint
from erlastic import port_connection, Atom
from car import Car
import time

WHITE = 255, 255, 255
BLUE = 0, 0, 255
RED = 255, 0, 0
BLACK = 0, 0, 0
GREY = 107, 107, 109

def display_graph(embeding: dict, roads: list) -> None:
    pygame.init()
    size = width, height = 500, 500 # 1000, 1000


    x_min, x_max, y_min, y_max = determine_bounds(embeding)
    x_offset, x_scale = determine_scale(x_min, x_max)
    y_offset, y_scale = determine_scale(y_min, y_max)

    screen = pygame.display.set_mode(size)
    visual_embeding = {}

    for inter, pos in embeding.items():

        x = (pos[0] * x_scale) + x_offset
        y = (pos[1] * y_scale) + y_offset

        visual_embeding[inter] = (x, y)

    draw_background(screen, visual_embeding, roads)
    listen_to_erlang(screen, visual_embeding, roads)
    
        
def listen_to_erlang(screen, visual_embeding, roads):
    messages, port = port_connection()
    port.send(Atom("go"))
    print("successful connection", file=sys.stderr, flush=True)

    cars = {}

    nexttime = time.time_ns() + 50000000

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()

        for pid, tag, data in messages:

            if tag == Atom("update"):
                if not pid in cars.keys():
                    cars[pid] = Car(data[0], str(data[1]), str(data[2]))
                else:
                    cars[pid].pos = data[0]
                    cars[pid].start = data[1]
                    cars[pid].end = data[2]
            elif tag == Atom("finished"):
                cars.pop(pid)
            else:
                print("fuck", file=sys.stderr)

            if time.time_ns() > nexttime:
                draw_background(screen, visual_embeding, roads)
                draw_cars(screen, cars, visual_embeding)
                pygame.display.flip()
                nexttime += 50000000

            if not cars: break
        break

def draw_background(screen, visual_embeding: dict, roads: list):
    screen.fill(GREY)

    for start, end in roads:
        pygame.draw.line(screen, RED, visual_embeding[start], visual_embeding[end], 4)

    for _inter, (x, y) in visual_embeding.items():
        pygame.draw.circle(screen, BLUE, (x, y), 10, 10)

def determine_bounds(embeding: dict):

    x_min = -1
    x_max = -1
    y_min = -1
    y_max = -1

    for x, y in embeding.values():
        if x < x_min or x_min == -1:
            x_min = x
        if x > x_max or x_max == -1:
            x_max = x
        if y < y_min or y_min == -1:
            y_min = y
        if y > y_max or y_max == -1:
            y_max = y

    return x_min, x_max, y_min, y_max

def determine_scale(min_val: float, max_val: float, target_width: int = 500):
    scale = (target_width * 0.9) / (max_val - min_val)
    offset = -min_val * scale + 50
    return offset, scale

def draw_car(c: Car, screen, visual_embeding) -> None:
    x_pos, y_pos = pos_from_data(visual_embeding, c.pos, c.start, c.end)

    pygame.draw.circle(screen, (0, 0, 0), (x_pos, y_pos), 5)

def pos_from_data(vis_emb: dict, prog: float, start: str, end: str) -> tuple:
    str_pos_x, str_pos_y = vis_emb[start]
    end_pos_x, end_pos_y = vis_emb[end]
    
    x_len = end_pos_x - str_pos_x
    y_len = end_pos_y - str_pos_y

    return (str_pos_x + (x_len * prog), str_pos_y + (y_len * prog))

def draw_cars(screen, cars: dict, visual_embeding: dict) -> None:
    for car in cars.values():
        draw_car(car, screen, visual_embeding)
