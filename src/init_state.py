import tomli as tomllib

class Init_State:
    def __init__(self, config_filename: str) -> None:
            with open(config_filename, "rb") as f:
                config = tomllib.load(f)
            self.intersections = config["intersections"]
            self.roads         = config["roads"]

    def __repr__(self):
        return f"{{intersections: {self.intersections},\nroads: {self.roads}}}"

if __name__ == "__main__":
    test = Init_State("roads.toml")
    print(test.intersections)
