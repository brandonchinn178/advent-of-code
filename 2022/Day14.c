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
    int max_y = -1;

    for (int i = 0; i < map->_length; i++) {
        int x = _from_map_index(i);
        MapColumn *col = &map->columns[i];
        for (int y = 0; y < col->_length; y++) {
            if (map_column_is_filled(col, y)) {
                if (y > max_y) max_y = y;
                if (x < min_x) min_x = x;
                if (x > max_x) max_x = x;
            }
        }
    }

    // add padding
    int min_y = 0;
    min_x -= 2;
    max_x += 2;
    max_y += 1;

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
                }
            }
        }
        prev_x = x, prev_y = y, is_first = false;
    }
}

static bool drop_sand(Map *map) {
    int x = SAND_START_X;
    int y = SAND_START_Y;
    while (true) {
        // printf("\33c\e[3J");
        // print_map_with_point(map, x, y);
        // usleep(10000);
        switch (map_get_occupant(map, x, y + 1)) {
            case ABYSS: return true;
            case EMPTY: {
                y = y + 1;
                break;
            }
            case ROCK:
            case SAND: {
                switch (map_get_occupant(map, x - 1, y + 1)) {
                    case ABYSS: return true;
                    case EMPTY: {
                        x = x - 1;
                        y = y + 1;
                        break;
                    }
                    case ROCK:
                    case SAND: {
                        switch (map_get_occupant(map, x + 1, y + 1)) {
                            case ABYSS: return true;
                            case EMPTY: {
                                x = x + 1;
                                y = y + 1;
                                break;
                            }
                            case ROCK:
                            case SAND: {
                                map_set_point(map, x, y, SAND);
                                return false;
                            }
                        }
                    }
                }
            }
        }
    }
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
    bool dropped_off_map = false;
    int sand_count = 0;
    while (true) {
        dropped_off_map = drop_sand(&map);
        if (dropped_off_map) {
            break;
        }
        sand_count++;
        // printf("\33c\e[3J");
        // printf("===== After dropping sand #%d =====\n", sand_count);
        // print_map(&map);
        // usleep(10000);
    }
    printf("Part 1: %d\n", sand_count);

    END_TIMER();
    return 0;
}
