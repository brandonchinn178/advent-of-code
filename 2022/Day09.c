#include "./utils.h"

// (0, 0) = start, +x = right, +y = up
typedef struct {
    int x;
    int y;
} Point;

#define POINT(x0, y0) ((Point) { .x = x0, .y = y0 })

static Point* copy_point(Point *point) {
    Point *copy = malloc(sizeof(Point));
    *copy = *point;
    return copy;
}

static void update_tail(Point head, Point *tail) {
    int dx = head.x - tail->x;
    int dy = head.y - tail->y;

    // touching
    if (abs(dx) <= 1 && abs(dy) <= 1) {
        return;
    }

    tail->x += dx > 0 ? 1 : dx < 0 ? -1 : 0;
    tail->y += dy > 0 ? 1 : dy < 0 ? -1 : 0;
}

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;

    Point head = POINT(0, 0), tail = POINT(0, 0);
    int x_min = 0, x_max = 0, y_min = 0, y_max = 0;

    List tail_positions = list_init(1000);
    list_append(&tail_positions, copy_point(&tail));

    while (get_line(&line, stdin) != -1) {
        char dir;
        int steps;
        sscanf(line, "%c %d", &dir, &steps);

        int dx = 0, dy = 0;
        switch (dir) {
            case 'R': dx = 1; break;
            case 'L': dx = -1; break;
            case 'U': dy = 1; break;
            case 'D': dy = -1; break;
        }

        for (int i = 0; i < steps; i++) {
            head.x += dx;
            head.y += dy;
            update_tail(head, &tail);
            if (head.x < x_min) x_min = head.x;
            if (head.x > x_max) x_max = head.x;
            if (head.y < y_min) y_min = head.y;
            if (head.y > y_max) y_max = head.y;

            list_append(&tail_positions, copy_point(&tail));
        }
    }

    int width = x_max - x_min;
    int height = y_max - y_min;
    bool seen[height][width];
    for (int j = 0; j < height; j++) {
        for (int i = 0; i < width; i++) {
            seen[j][i] = false;
        }
    }

    int unique_tail_positions = 0;
    for (int i = 0; i < tail_positions.length; i++) {
        Point pos = *((Point *) list_get(tail_positions, i));
        int x = x_min + pos.x, y = y_min + pos.y;
        if (seen[y][x]) {
            continue;
        }
        seen[y][x] = true;
        unique_tail_positions++;
    }
    printf("Part 1: %d\n", unique_tail_positions);

    END_TIMER();
    return 0;
}
