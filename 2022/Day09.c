#include "./utils.h"

// (0, 0) = start, +x = right, +y = up
typedef struct {
    int x;
    int y;
} Point;

static Point origin_point = (Point) { .x = 0, .y = 0 };

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

    Point knots[10];
    for (int i = 0; i < 10; i++) {
        knots[i] = origin_point;
    }
    Point *head = &knots[0];
    Point *tail1 = &knots[1];
    Point *tail9 = &knots[9];

    int x_min = 0, x_max = 0, y_min = 0, y_max = 0;

    List positions_tail1 = list_init(1000);
    List positions_tail9 = list_init(1000);
    list_append(&positions_tail1, &origin_point);
    list_append(&positions_tail9, &origin_point);

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
            head->x += dx;
            head->y += dy;
            for (int j = 1; j < 10; j++) {
                update_tail(knots[j - 1], &knots[j]);
            }

            list_append(&positions_tail1, copy_point(tail1));
            list_append(&positions_tail9, copy_point(tail9));
        }
        if (head->x < x_min) x_min = head->x;
        if (head->x > x_max) x_max = head->x;
        if (head->y < y_min) y_min = head->y;
        if (head->y > y_max) y_max = head->y;
    }

    int width = x_max - x_min;
    int height = y_max - y_min;
    bool seen1[height][width], seen9[height][width];
    for (int j = 0; j < height; j++) {
        for (int i = 0; i < width; i++) {
            seen1[j][i] = false;
            seen9[j][i] = false;
        }
    }

    int unique_tail1_positions = 0;
    int unique_tail9_positions = 0;
    for (int i = 0; i < positions_tail1.length; i++) {
        Point *pos1 = list_get(positions_tail1, i);
        int x1 = x_min + pos1->x, y1 = y_min + pos1->y;
        if (!seen1[y1][x1]) {
            seen1[y1][x1] = true;
            unique_tail1_positions++;
        }
        Point *pos9 = list_get(positions_tail9, i);
        int x9 = x_min + pos9->x, y9 = y_min + pos9->y;
        if (!seen9[y9][x9]) {
            seen9[y9][x9] = true;
            unique_tail9_positions++;
        }
    }
    printf("Part 1: %d\n", unique_tail1_positions);
    printf("Part 2: %d\n", unique_tail9_positions);

    END_TIMER();
    return 0;
}
