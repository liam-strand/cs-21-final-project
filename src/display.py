"""
display.py

CS 21 Final Project
on-the-road-again
April 2022

This file contains the logic for listening to messages from the erlang port and
updates the visualization to match the state changes.

The pygame visuals are initilized along with the erlang listener in this file.

"""

# Everything from stdout gets sent to the Erlang program that spawned
# this python process, as part of the port driver abstraction. This
# means that any erroneous output will confuse the Erlang server and
# effectively break the whole program. Normally, pygame prints a
# message to stdout when you import it, so we have to set this
# environment variable in order to suppress that message.
from os import environ
environ["PYGAME_HIDE_SUPPORT_PROMPT"] = "hide"

import sys

from erpy import stdio_port_connection
from term import Atom
import pygame

from car import Car
from time import time_ns as time_ns
from math import sin, cos, pi, atan2 as atan, degrees, acos, radians

# Display Settings
RESOLUTION  = 1000
FRAMERATE   = 30
UPDATE_TIME = 1000000000 // FRAMERATE
RWIDTH = 17

# Color Constants
WHITE = 255, 255, 255
BLUE  =   0,   0, 255
RED   = 255,   0,   0
BLACK =   0,   0,   0
GREY  = 107, 107, 109
GREEN =   0, 255,   0
GREY2 = 160, 160, 160
RCOLOR = 66,66,66
YELLOW = 172, 172, 58

def display(embeding: dict, roads: list) -> None:
    """Initialize and update the display based on recieved messages
    Parameters: 
          embeding: A map of intersections to initial positions
             roads: A list of roads connecting intersections
       Returns: None
       Effects: Displays window
    """

    pygame.init()
    screen = pygame.display.set_mode((RESOLUTION, RESOLUTION))

    # Calculate necessary adjustments to rescale graph embeding 
    x_min, x_max, y_min, y_max = determine_bounds(embeding)
    x_offset, x_scale = determine_scale(x_min, x_max)
    y_offset, y_scale = determine_scale(y_min, y_max)

    # Perform the rescale
    visual_embeding = {}
    for inter, pos in embeding.items():
        x = (pos[0] * x_scale) + x_offset
        y = (pos[1] * y_scale) + y_offset

        visual_embeding[inter] = (x, y)

    listen_to_erlang(screen, visual_embeding, roads)
    
        
def listen_to_erlang(screen, visual_embeding, roads) -> None:
    """Listen to messages from erlang and update visualization to math
    Parameters: 
                screen: the screen to display the images
       visual_embeding: the map generated earlier, representing the positions
                        of the intersections
                 roads: a list of the connections between intersections
       Returns: None
       Effects: Displays visualization
    """
    
    # Set up the connection to erlang port (aka stdin and stdout)
    inbox, port = stdio_port_connection()
    port.send(Atom("go"))
    print("successful connection", file=sys.stderr, flush=True)

    cars = {}
    done = []

    # Initilize display clock
    nexttime = time_ns() + UPDATE_TIME

    # This preamble is to trick pygame into displaying the window, but
    # we sidestep the event loop by only running the loop once, then using our
    # own event management system.
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit()

        # inbox is a generator that produces messages when they arrive
        # our message format insists that messages are a 3-tuple of form...
        #       {sender's PID (string), data tag (Atom), data (Any)}
        # the pid is a string because erlastic doesn't understand the new
        # erlang pid binary encoding
        for msg in inbox:
            match msg:
                case (pid, tag, data):
                    # If we get a car's update, we update their location in our
                    # internal representation. If it is a new car we add it.
                    if tag == Atom("update"):
                        if not pid in cars.keys():
                            cars[pid] = Car(data[0], str(data[1]), str(data[2]))
                        else:
                            cars[pid].pos = min(data[0], 0.95)
                            cars[pid].start = data[1]
                            cars[pid].end = data[2]
                    
                    # If a car tells us it has reached its destination, we remove it
                    # from the internal representation. We also mark the final
                    # destination of the car, so we can make it flash on the next
                    # screen update.
                    elif tag == Atom("finished"):
                        done.append(visual_embeding[cars[pid].end])
                        cars.pop(pid)
                    
                    elif tag == Atom("waiting"):
                        cars[pid].pos = 0.95

                    elif tag == Atom("tick"):
                        pass

                    # If we get a message that we don't understand we emit an expletive
                    else:
                        print("crap", file=sys.stderr, flush=True)

                    # If it is time to update the display, we do so and set the timer
                    # for 1 // FRAMERATE seconds.
                    if time_ns() > nexttime:
                        draw_background(screen, visual_embeding, roads)
                        draw_cars(screen, cars, visual_embeding)
                        draw_done(screen, done)
                        pygame.display.flip()
                        nexttime += UPDATE_TIME

                    # When every car has reached its destination, we tell the erlang
                    # port that we are done, then terminate the loop.
                    if not cars: 
                        port.send(Atom("stop"))
                        break
                
        # Again, this outer loop is just boilerplate to convince pygame that
        # we are paying attention to their event loop. We are not.
        break

def draw_background(screen, visual_embeding: dict, roads: list) -> None:
    """Draw the roads and intersections.
    Parameters: 
                screen: a pygame display we can draw on
       visual_embeding: the map generated earlier, representing the positions
                        of the intersections
                 roads: a list of the connections between intersections
       Returns: None
       Effects: Draw the roads and intersections on the display
    """
    screen.fill(WHITE)

    for start, end in roads:
        #draw_road(screen,visual_embeding[start], visual_embeding[end])
        pygame.draw.line(screen, GREY2, visual_embeding[start],
            visual_embeding[end], 28)
        pygame.draw.line(screen, RCOLOR, visual_embeding[start],
            visual_embeding[end], 24)
        pygame.draw.line(screen, YELLOW, visual_embeding[start],
            visual_embeding[end], 2)

    for x, y in visual_embeding.values():
        pygame.draw.circle(screen, RCOLOR, (x, y), 25, 25)
        pygame.draw.circle(screen, WHITE, (x, y), 4, 4)

def determine_bounds(embeding: dict) -> tuple:
    """Figure out the dimensions of the initial graph embeding.
    Parameters: 
          embeding: A dictionary mapping the intersection names to their
                    initial position
       Returns: A 4-tuple of the form (x_min, x_max, y_min, y_max)
       Effects: None
    """
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
    """Figure out the transformations necessary to rescale the embeding
    Parameters: 
           min_val: The minimum value in a given dimension
           max_val: The maximum value in a given dimension
       Returns: A 2-tuple of the form (offset, scale) that can be used to
                rescale embeded coordinates in a given dimension
       Effects: None
    """
    scale = (RESOLUTION * 0.9) / (max_val - min_val)
    offset = (-min_val * scale) + (RESOLUTION * 0.1 / 2)
    return offset, scale


def rotate(x, y: float, theta: float) -> tuple:
    """ Rotate a point counterclockwise about the origin.
    Parameters:
           x, y:  coordinates of a point
           theta: angle of rotation, in radians
    Returns:
           Coordinates of (x, y) rotated counterclockwise
           about the point (0, 0), by angle theta.
    Effects: None
    """
    return (x * cos(theta) - y * sin(theta),
            x * sin(theta) + y * cos(theta))



def draw_car(c: Car, screen, embed) -> None:
    """Draw a simple representation of a car on a road.
    Parameters:
         c:      a Car object
         screen: a pygame display
         embed:  a digraph representation of the road system
    Notes:
         The `start' and `end' fields of `c' should be valid
         intersections within `embed', with an edge going from `start'
         to `end'.
    Effects:
         Draws a representation of `c' onto `screen'.  `c' is depicted
         as a white isosceles triangle with a black outline, situated
         between its start and end intersections, with its longer end
         pointed towards the end intersection.
    """
    x_pos, y_pos = pos_from_data(embed, c.pos, c.start, c.end)

    # draw an isosceles triangle pointed towards destination!
    x0, y0 = embed[c.start]
    x1, y1 = embed[c.end]
    theta = atan(y1 - y0, x1 - x0) + (pi / 2)
    offset_x = cos(theta) * 6
    offset_y = sin(theta) * 6 
    dx0, dy0 = rotate(0, -8, theta)
    dx1, dy1 = rotate(4, 4, theta)
    dx2, dy2 = rotate(-4, 4, theta)
    pygame.draw.polygon(screen, WHITE,
                        [[x_pos + dx0 + offset_x, y_pos + dy0 + offset_y],
                         [x_pos + dx1 + offset_x, y_pos + dy1 + offset_y],
                         [x_pos + dx2 + offset_x, y_pos + dy2 + offset_y]])
    pygame.draw.polygon(screen, BLACK,
                        [[x_pos + dx0 + offset_x, y_pos + dy0 + offset_y],
                         [x_pos + dx1 + offset_x, y_pos + dy1 + offset_y],
                         [x_pos + dx2 + offset_x, y_pos + dy2 + offset_y]], 1)


def draw_done(screen, pos_list) -> None:
    """Puts a little green dot on every intersection where a car terminated
    Parameters: 
           screen: a pygame display we can draw on
         pos_list: a list of the position of every intersection where a car has
                   terminated its journey
       Returns: None
       Effects: Makes pos_list empty
    """
    for pos in pos_list:
        pygame.draw.circle(screen, GREEN, pos, 5)

    pos_list.clear()

def pos_from_data(vis_emb: dict, prog: float, start: str, end: str) -> tuple:
    """Get the position of a car based on its progress between two intersections
    Parameters: 
          vis_embed: A dict mapping intersections to their position
               prog: The progress of a car between two nodes, in [0, 1]
              start: The name of the starting intersection
                end: The name of the ending intersection
       Returns: A 2-tuple of the form (x_position, y_position)
       Effects: None
    """
    str_pos_x, str_pos_y = vis_emb[start]
    end_pos_x, end_pos_y = vis_emb[end]
    
    x_len = end_pos_x - str_pos_x
    y_len = end_pos_y - str_pos_y

    return (str_pos_x + (x_len * prog), str_pos_y + (y_len * prog))

def draw_cars(screen, cars: dict, visual_embeding: dict) -> None:
    """Draws all the cars on the screen
    Parameters: 
             screen: a pygame display we can draw on
               cars: A dictionary mapping cars pids to a Car class
    visual_embeding: A dictionary mapping intersections to their locations
       Returns: None
       Effects: Draws all the cars on the display
    """
    for car in cars.values():
        draw_car(car, screen, visual_embeding)

