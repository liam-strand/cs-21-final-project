"""
display.py

CS 21 Final Project
on-the-road-again
April 2022

This file contains the logic for listening to messages from the erlang port and
updates the visualization to match the state changes.

The pygame visuals are initilized along with the erlang listener in this file.

"""

# IMPORTANT NOTE:
# Everything from stdout gets sent to the Erlang program that spawned
# this python process, as part of the port driver abstraction. This
# means that any erroneous output will confuse the Erlang server and
# effectively break the whole program. Normally, pygame prints a
# message to stdout when you import it, so we have to set this
# environment variable in order to suppress that message.

import sys
from os import environ

from erlastic import port_connection, Atom
import pygame

from car import Car
from time import time_ns as time_ns
from math import sin, cos, pi, atan2 as atan

# Display Settings
RESOLUTION = 1000
FRAMERATE  = 30

# Color Constants
WHITE = 255, 255, 255
BLUE  =   0,   0, 255
RED   = 255,   0,   0
BLACK =   0,   0,   0
GREY  = 107, 107, 109
GREEN =   0, 255,   0

def display(embeding: dict, roads: list) -> None:
    
    environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"
    pygame.init()

    size = RESOLUTION, RESOLUTION

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
    done = []

    nexttime = time_ns() + (1000000000 // FRAMERATE)

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
                done.append(visual_embeding[cars[pid].end])
                cars.pop(pid)
            else:
                print("fuck", file=sys.stderr)

            if time_ns() > nexttime:
                draw_background(screen, visual_embeding, roads)
                draw_cars(screen, cars, visual_embeding)
                draw_done(screen, done)
                pygame.display.flip()
                nexttime += 50000000

            if not cars: 
                port.send(Atom("stop"))
                break
        
        break

def draw_background(screen, visual_embeding: dict, roads: list):
    screen.fill(WHITE)

    for start, end in roads:
        pygame.draw.line(screen, BLACK, visual_embeding[start],
                         visual_embeding[end], 1)

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

def determine_scale(min_val: float, max_val: float):
    scale = (RESOLUTION * 0.9) / (max_val - min_val)
    offset = (-min_val * scale) + (RESOLUTION * 0.1 / 2)
    return offset, scale


def rotate(x, y, theta):
    return (x * cos(theta) - y * sin(theta),
            x * sin(theta) + y * cos(theta))


def draw_car(c: Car, screen, embed) -> None:
    x_pos, y_pos = pos_from_data(embed, c.pos, c.start, c.end)
    # draw an isosceles triangle pointed towards destination!
    x0, y0 = embed[c.start]
    x1, y1 = embed[c.end]
    theta = atan(y1 - y0, x1 - x0) + (pi / 2)
    dx0, dy0 = rotate(0, -8, theta)
    dx1, dy1 = rotate(4, 4, theta)
    dx2, dy2 = rotate(-4, 4, theta)
    pygame.draw.polygon(screen, WHITE,
                        [[x_pos + dx0, y_pos + dy0],
                         [x_pos + dx1, y_pos + dy1],
                         [x_pos + dx2, y_pos + dy2]])
    pygame.draw.polygon(screen, BLACK,
                        [[x_pos + dx0, y_pos + dy0],
                         [x_pos + dx1, y_pos + dy1],
                         [x_pos + dx2, y_pos + dy2]], 1)

def draw_done(screen, pos_list) -> None:
    while pos_list:
        pygame.draw.circle(screen, GREEN, pos_list[0], 5)
        pos_list.pop(0)

def pos_from_data(vis_emb: dict, prog: float, start: str, end: str) -> tuple:
    str_pos_x, str_pos_y = vis_emb[start]
    end_pos_x, end_pos_y = vis_emb[end]
    
    x_len = end_pos_x - str_pos_x
    y_len = end_pos_y - str_pos_y

    return (str_pos_x + (x_len * prog), str_pos_y + (y_len * prog))

def draw_cars(screen, cars: dict, visual_embeding: dict) -> None:
    for car in cars.values():
        draw_car(car, screen, visual_embeding)
