class Car:
    def __init__(self, pos: float, start: str, end: str):
        self.pos = pos
        self.start = start
        self.end = end

    def __repr__(self):
        return f"pos {self.pos} on road from {self.start} to {self.end}"
