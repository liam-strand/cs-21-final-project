# IMPORTANT NOTE:
#
# Everything from stdout gets sent to the Erlang program that spawned
# this python process, as part of the port driver abstraction.  This
# means that any erroneous output will confuse the Erlang server and
# effectively break the whole program.  Normally, pygame prints a
# message to stdout when you import it, so we have to set this
# environment variable in order to suppress that message.
import os
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = "hide"


import pygame
import sys

from pprint import pprint
# from erlastic import port_connection, Atom
from erlastic import port_connection, Atom
from car import Car

WHITE = 255, 255, 255
BLUE = 0, 0, 255
RED = 255, 0, 0
BLACK = 0, 0, 0

def display_graph(embeding: dict, roads: list) -> None:
    pygame.init()
    size = width, height = 500, 500

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
    print("successful connection", file=sys.stderr, flush=True)

    
    cars = {}

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()

        # for pid, tag, data in messages:
        for tag, data in messages:
            draw_background(screen, visual_embeding, roads)

            if tag == Atom("update"):
                update_cars(cars, data)
            elif tag == Atom("finished"):
                cars.pop(pid)
            else:
                print("oof", file=sys.stderr)
                
            port.send(Atom("go"))
            print("processed messages", file=sys.stderr)
            draw_cars(screen, cars, visual_embeding)
            print("drew cars", file=sys.stderr)

            pygame.display.flip()
            print("flipped display", file=sys.stderr)
        break

def update_cars(cars, update):
    for data in update:
        pid, (start, end), pos = data
        print(pid, start, end, pos, file=sys.stderr)
        cars[str(pid)] = Car(pos, start, end)
        # key = str(pid)
        # if not str(pid) in cars.keys():
        #     cars[key] = Car(pos, start, end)
        # else:
        #     cars[key].pos   = pos
        #     cars[key].start = start
        #     cars[key].end   = end



def draw_background(screen, visual_embeding: dict, roads: list):
    screen.fill(WHITE)

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

def determine_scale(min_val: float, max_val: float, target_width: int = 1000):
    scale = (target_width * 0.8) / (max_val - min_val)
    offset = -min_val * scale + 100
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
