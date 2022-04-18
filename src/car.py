class Car:
    def __init__(self, start: str, end: str, speed: float):
        self.start = start
        self.end = end
        self.speed = speed
        self.loc = ((start, end), 0)

    def __repr__(self):
        return f"pos {self.loc[1]} on road from {self.start} to {self.end} at speed {self.speed}"
