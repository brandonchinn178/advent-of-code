import collections
import enum
import math
from pathlib import Path

class Occupant(enum.Enum):
    ROCK = enum.auto()
    SAND = enum.auto()

SAND_START_X = 500
SAND_START_Y = 0

class Grid:
    def __init__(self):
        self._grid = collections.defaultdict(dict)

        self._min_x = math.inf
        self._max_x = -math.inf
        self._max_y = -math.inf

    def add(self, point, occupant):
        x, y = point
        self._grid[x][y] = occupant
        self._min_x = min(x, self._min_x)
        self._max_x = max(x, self._max_x)
        self._max_y = max(y, self._max_y)

    def display(self):
        self.display_with_sand(None)

    def display_with_sand(self, point):
        min_y = 0
        min_x = self._min_x - 2
        max_x = self._max_x + 2
        max_y = self._max_y + 1

        print(f"Window: ({min_x}, 0) -> ({max_x}, {max_y})")
        for y in range(min_y, max_y + 1):
            row = []
            for x in range(min_x, max_x + 1):
                if x == SAND_START_X and y == SAND_START_Y:
                    row.append("↧")
                else:
                    occupant = Occupant.SAND if (x, y) == point else self._grid[x].get(y)
                    if occupant == Occupant.ROCK:
                        row.append("█")
                    elif occupant == Occupant.SAND:
                        row.append("•")
                    else:
                        row.append(" ")
            print("".join(row))

def main():
    grid = Grid()

    for line in Path("data/Day14.txt").read_text().splitlines():
        endpoints = [tuple(map(int, s.split(","))) for s in line.split(" -> ")]
        for (x1, y1), (x2, y2) in zip(endpoints, endpoints[1:]):
            x1, x2 = min(x1, x2), max(x1, x2)
            y1, y2 = min(y1, y2), max(y1, y2)
            for x in range(x1, x2 + 1):
                for y in range(y1, y2 + 1):
                    grid.add((x, y), Occupant.ROCK)

    grid.display()

if __name__ == "__main__":
    main()
