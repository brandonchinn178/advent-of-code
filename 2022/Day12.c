#include "./utils.h"

typedef struct Point {
    char label;
    int x;
    int y;
} Point;

int main(int argc, char **argv) {
    START_TIMER();

    // grid[y][x] gets the character at the (x, y) coordinate,
    // where (0, 0) is the top-left corner
    Point **grid = malloc(sizeof(Point*));
    int width = 0, height = 0;
    Point *start, *end;

    char *line = NULL;
    size_t line_len;
    while ((line_len = get_line(&line, stdin)) != -1) {
        if (width == 0) {
            width = line_len;
        }
        height++;

        grid = realloc(grid, height * sizeof(Point*));
        int y = height - 1;
        grid[y] = malloc(width * sizeof(Point));
        for (int x = 0; x < width; x++) {
            char c = line[x];
            grid[y][x] = (Point) { .label = c, .x = x, .y = y };
            if (c == 'S') {
                start = &grid[y][x];
                start->label = 'a';
            } else if (c == 'E') {
                end = &grid[y][x];
                end->label = 'z';
            }
        }
    }

    bool seen[height][width];
    for (int j = 0; j < height; j++) {
        for (int i = 0; i < width; i++) {
            seen[j][i] = false;
        }
    }

    List buf = list_empty();
    list_append(&buf, end);
    List buf_next = list_empty();
    int dist = 0, part1_dist = -1, part2_dist = -1;
    while (buf.length > 0) {
        for (int i = 0; i < buf.length; i++) {
            Point *point = list_get(buf, i);
            char c = point->label;
            int x = point->x, y = point->y;

            if (c == 'a' && part2_dist == -1) {
                part2_dist = dist;
            }
            if (point == start) {
                part1_dist = dist;
                goto end;
            }
            if (seen[y][x]) {
                continue;
            }

            seen[y][x] = true;

            for (int j = 0; j < 4; j++) {
                int x2 = x, y2 = y;
                switch (j) {
                    // up
                    case 0: y2--; break;
                    // down
                    case 1: y2++; break;
                    // left
                    case 2: x2--; break;
                    // right
                    case 3: x2++; break;
                }
                if (y2 >= 0 && y2 < height && x2 >= 0 && x2 < width) {
                    Point *neighbor = &grid[y2][x2];
                    if (!seen[y2][x2] && neighbor->label >= c - 1) {
                        list_append(&buf_next, neighbor);
                    }
                }
            }
        }

        // swap buf + buf_next, reset buf_next
        List tmp = buf;
        buf = buf_next;
        buf_next = tmp;
        buf_next.length = 0;

        dist++;
    }

    end:
    printf("Part 1: %d\n", part1_dist);
    printf("Part 2: %d\n", part2_dist);

    END_TIMER();
    return 0;
}
