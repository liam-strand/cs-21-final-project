import pygame
import sys
from pprint import pprint

def display_graph(embeding: dict, roads: list) -> None:
    pygame.init()
    size = width, height = 1000, 1000
    WHITE = 255, 255, 255
    BLUE  =   0,   0, 255

    x_min, x_max, y_min, y_max = determine_bounds(embeding)
    x_offset, x_scale = determine_scale(x_min, x_max)
    y_offset, y_scale = determine_scale(y_min, y_max)

    print(f"x_min: {x_min}, x_max: {x_max}, y_min: {y_min}, y_max: {y_max}")
    print(f"x_offset: {x_offset}, x_scale: {x_scale}")
    print(f"y_offset: {y_offset}, y_scale: {y_scale}")

    screen = pygame.display.set_mode(size)

    screen.fill(WHITE)
    pygame.display.update()

    visual_embeding = {}

    for inter, pos in embeding.items():
        
        x = (pos[0] * x_scale) + x_offset
        y = (pos[1] * y_scale) + y_offset

        visual_embeding[inter] = (x, y)

        pygame.draw.circle(screen, BLUE, (x, y), 10, 10)

    pygame.display.update()

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT: sys.exit()


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
    offset = -min_val * scale
    return offset, scale
