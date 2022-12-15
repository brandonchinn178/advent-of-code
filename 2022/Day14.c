#include <unistd.h>
#include "./utils.h"

#define SAND_START_X 500
#define SAND_START_Y 0

typedef enum {
    EMPTY,
    ROCK,
    SAND,
    ABYSS,
} Occupant;

typedef struct {
    Occupant *occupants;
    size_t _length;
} MapColumn;

static MapColumn init_map_column() {
    size_t initial_capacity = 100;
    Occupant *occupants = malloc(initial_capacity * sizeof(Occupant));
    for (int i = 0; i < initial_capacity; i++) {
        occupants[i] = EMPTY;
    }
    return (MapColumn) {
        .occupants = occupants,
        ._length = initial_capacity,
    };
}

static MapColumn copy_map_column(MapColumn col) {
    size_t length = col._length;
    Occupant *occupants = malloc(length * sizeof(Occupant));
    for (int i = 0; i < length; i++) {
        occupants[i] = col.occupants[i];
    }
    return (MapColumn) { .occupants = occupants, ._length = length };
}

static Occupant map_column_get_occupant(MapColumn *col, int y) {
    if (y >= col->_length) {
        return ABYSS;
    }
    return col->occupants[y];
}

static bool map_column_is_filled(MapColumn *col, int y) {
    switch (map_column_get_occupant(col, y)) {
        case EMPTY: return false;
        case ROCK: return true;
        case SAND: return true;
        case ABYSS: return false;
    }
}

static void map_column_set_point(MapColumn *col, int y, Occupant occupant) {
    // resize if necessary
    size_t old_length = col->_length;
    if (y >= old_length) {
        col->_length = y * 2;
        col->occupants = realloc(col->occupants, col->_length * sizeof(Occupant));
        for (int i = old_length; i < col->_length; i++) {
            col->occupants[i] = EMPTY;
        }
    }

    col->occupants[y] = occupant;
}

#define MAP_CENTER 500
#define _to_map_index(x) (x >= MAP_CENTER ? (x - MAP_CENTER) * 2 : (MAP_CENTER - x) * 2 - 1)
#define _from_map_index(i) (i % 2 == 0 ? MAP_CENTER + i / 2 : MAP_CENTER - i / 2 - 1)

typedef struct {
    // a list of MapColumns corresponding to the x-coordinates:
    // {MAP_CENTER, MAP_CENTER - 1, MAP_CENTER + 1, ...}
    MapColumn *columns;
    size_t _length;

    int max_y;
} Map;

static Map init_map() {
    size_t initial_capacity = 150;
    MapColumn *columns = malloc(initial_capacity * sizeof(MapColumn));
    for (int i = 0; i < initial_capacity; i++) {
        columns[i] = init_map_column();
    }
    return (Map) {
        .columns = columns,
        ._length = initial_capacity,
        .max_y = 0,
    };
}

static Map copy_map(Map map) {
    size_t length = map._length;
    MapColumn *columns = malloc(length * sizeof(MapColumn));
    for (int i = 0; i < length; i++) {
        columns[i] = copy_map_column(map.columns[i]);
    }
    return (Map) {
        .columns = columns,
        ._length = length,
        .max_y = map.max_y,
    };
}

static Occupant map_get_occupant(Map *map, int x, int y) {
    int i = _to_map_index(x);
    MapColumn *col = &map->columns[i];
    return map_column_get_occupant(col, y);
}

static void map_set_point(Map *map, int x, int y, Occupant occupant) {
    int i = _to_map_index(x);

    // resize if necessary
    size_t old_length = map->_length;
    if (i >= old_length) {
        map->_length = i * 2;
        map->columns = realloc(map->columns, map->_length * sizeof(MapColumn));
        for (int i = old_length; i < map->_length; i++) {
            map->columns[i] = init_map_column();
        }
    }

    // set point in column
    MapColumn *col = &map->columns[i];
    map_column_set_point(col, y, occupant);
}

static void print_map_with_point(Map *map, int extra_x, int extra_y) {
    int min_x = 10000;
    int max_x = -1;
    int min_y = 0;
    int max_y = map->max_y + 2;

    for (int i = 0; i < map->_length; i++) {
        int x = _from_map_index(i);
        MapColumn *col = &map->columns[i];
        for (int y = 0; y < col->_length; y++) {
            if (map_column_is_filled(col, y)) {
                if (x < min_x) min_x = x;
                if (x > max_x) max_x = x;
            }
        }
    }

    // add padding
    min_x -= 2;
    max_x += 2;

    printf("Window: (%d, %d) -> (%d, %d)\n", min_x, min_y, max_x, max_y);
    for (int y = min_y; y <= max_y; y++) {
        for (int x = min_x; x <= max_x; x++) {
            if (x == SAND_START_X && y == SAND_START_Y) {
                printf("↧");
                continue;
            }
            Occupant occupant =
                (x == extra_x && y == extra_y)
                    ? SAND
                    : map_get_occupant(map, x, y);

            switch (occupant) {
                case ABYSS:
                case EMPTY:
                    printf(" ");
                    break;
                case ROCK:
                    printf("█");
                    break;
                case SAND:
                    printf("•");
                    break;
            }
        }
        printf("\n");
    }
}

static void print_map(Map *map) {
    print_map_with_point(map, -1, -1);
}

/***** Entrypoint *****/

static void add_points(Map *map, char *line, size_t line_len) {
    bool is_first = true;
    int prev_x, prev_y;

    int i = 0;
    while (i < line_len) {
        int x = 0;
        while (line[i] != ',') {
            x = x * 10 + (line[i] - '0');
            i++;
        }

        // skip comma
        i++;

        int y = 0;
        while (i < line_len && line[i] != ' ') {
            y = y * 10 + (line[i] - '0');
            i++;
        }

        // skip " -> "
        i += 4;

        if (!is_first) {
            int x1 = x < prev_x ? x : prev_x;
            int x2 = x < prev_x ? prev_x : x;
            int y1 = y < prev_y ? y : prev_y;
            int y2 = y < prev_y ? prev_y : y;
            for (int i = x1; i <= x2; i++) {
                for (int j = y1; j <= y2; j++) {
                    map_set_point(map, i, j, ROCK);
                    if (j > map->max_y) map->max_y = j;
                }
            }
        }
        prev_x = x, prev_y = y, is_first = false;
    }
}

typedef union {
    bool dropped_off_map;
    struct {
        bool dropped_off_map;
        int x, y;
    } new_point;
} RestingPoint;

static RestingPoint drop_sand(Map *map, bool infinite_abyss) {
    int x = SAND_START_X;
    int y = SAND_START_Y;
    int deltas[3][2] = {
        {0, 1},
        {-1, 1},
        {1, 1}
    };

loop:
    // printf("\33c\e[3J");
    // print_map_with_point(map, x, y);
    // usleep(10000);
    for (int i = 0; i < 3; i++) {
        int next_x = x + deltas[i][0];
        int next_y = y + deltas[i][1];
        Occupant occupant =
            (!infinite_abyss && next_y == map->max_y + 2)
                ? ROCK
                : map_get_occupant(map, next_x, next_y);
        switch (occupant) {
            case ROCK:
            case SAND:
                // try next delta
                break;
            case ABYSS:
                if (infinite_abyss) {
                    return (RestingPoint) { .dropped_off_map = true };
                }
                // fallthrough to EMPTY
            case EMPTY:
                x = next_x;
                y = next_y;
                goto loop;
        }
    }

    map_set_point(map, x, y, SAND);
    return (RestingPoint) {
        .new_point = {
            .dropped_off_map = false,
            .x = x,
            .y = y,
        },
    };

}

int main(int argc, char **argv) {
    START_TIMER();

    Map map = init_map();

    char *line = NULL;
    size_t line_len;
    while ((line_len = get_line(&line, stdin)) != -1) {
        add_points(&map, line, line_len);
    }

    // printf("===== After parsing =====\n");
    // print_map(&map);

    // part 1
    Map part1_map = copy_map(map);
    int part1_count = 0;
    while (true) {
        RestingPoint resting_point = drop_sand(&part1_map, true);
        if (resting_point.dropped_off_map) {
            break;
        }
        part1_count++;
        // printf("\33c\e[3J");
        // printf("===== After dropping sand #%d =====\n", part1_count);
        // print_map(&part1_map);
        // usleep(10000);
    }
    printf("Part 1: %d\n", part1_count);

    // part 2
    Map part2_map = copy_map(map);
    int part2_count = 0;
    while (true) {
        RestingPoint resting_point = drop_sand(&part2_map, false);
        part2_count++;
        int resting_x = resting_point.new_point.x;
        int resting_y = resting_point.new_point.y;
        if (resting_x == SAND_START_X && resting_y == SAND_START_Y) {
            break;
        }
        // printf("\33c\e[3J");
        // printf("===== After dropping sand #%d =====\n", part2_count);
        // print_map(&part2_map);
        // usleep(10000);
    }
    printf("Part 2: %d\n", part2_count);

    END_TIMER();
    return 0;
}
