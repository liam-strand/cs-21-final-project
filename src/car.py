"""
car.py

CS 21 Final Project
on-the-road-again
April 2022

This class provides a representation of a Car. Position is represented as a
value between 0 and 1 between the start and end strings.

"""

class Car:
    def __init__(self, pos: float, start: str, end: str):
        self.pos = pos
        self.start = start
        self.end = end

    def __repr__(self):
        return f"{self.pos:.0%} of the way from {self.start} to {self.end}"
