import pygame
import sys
from pprint import pprint
from erlastic import port_connection, Atom

WHITE = 255, 255, 255
BLUE = 0, 0, 255
RED = 255, 0, 0
BLACK = 0, 0, 0

def display_graph(embeding: dict, roads: list) -> None:
    pygame.init()
    size = width, height = 1000, 1000

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

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()

        for tag, data in messages:

            draw_background(screen, visual_embeding, roads)

            if tag == Atom("update"):
                pygame.draw.circle(screen, BLACK, (data[0], data[1]), 5, 5)
            else:
                print("fuck", file=sys.stderr)

            pygame.display.flip()

        sys.exit()

def draw_background(screen, visual_embeding: dict, roads: list):
    screen.fill(WHITE)

    for start, end in roads:
        pygame.draw.line(screen, RED, visual_embeding[start], visual_embeding[end], 4)

    for _inter, (x, y) in visual_embeding.items():
        pygame.draw.circle(screen, BLUE, (x, y), 10, 10)

    pygame.display.update()

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
