#include "./utils.h"

/***** Point *****/

// (0, 0) = start, +x = right, +y = up
typedef struct {
    int x;
    int y;
} Point;

static Point origin_point = (Point) { .x = 0, .y = 0 };

/***** Points *****/

typedef struct {
    Point* contents;
    size_t length;
    size_t _contents_size;
} Points;

Points points_init() {
    size_t size = 1000;
    Point *contents = malloc(size * sizeof(Point));
    return (Points) {
        .contents = contents,
        .length = 0,
        ._contents_size = size,
    };
}

void points_append(Points* points, Point p) {
    if (points->length == points->_contents_size) {
        points->_contents_size *= 2;
        points->contents = realloc(points->contents, points->_contents_size * sizeof(void*));
    }
    points->contents[points->length] = p;
    points->length++;
}

Point points_get(Points points, size_t index) {
    return points.contents[index];
}

/***** Entrypoint *****/

static Point next_tail_pos(Point head, Point tail) {
    int dx = head.x - tail.x;
    int dy = head.y - tail.y;

    // touching
    if (abs(dx) <= 1 && abs(dy) <= 1) {
        return tail;
    }

    return (Point) {
        .x = tail.x + (dx > 0 ? 1 : dx < 0 ? -1 : 0),
        .y = tail.y + (dy > 0 ? 1 : dy < 0 ? -1 : 0),
    };
}

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;

    // first, keep track of all head positions +
    // find boundaries of map

    Point head = origin_point;

    Points head_positions = points_init();
    points_append(&head_positions, head);

    int x_min = 0, x_max = 0, y_min = 0, y_max = 0;

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
            points_append(&head_positions, head);
        }
        if (head.x < x_min) x_min = head.x;
        if (head.x > x_max) x_max = head.x;
        if (head.y < y_min) y_min = head.y;
        if (head.y > y_max) y_max = head.y;
    }

    // then track a boolean matrix + counter for unique
    // positions of the  first knot (part 1) and the
    // last knot (part 2)

    int width = x_max - x_min;
    int height = y_max - y_min;
    bool seen1[height][width];
    bool seen9[height][width];
    for (int j = 0; j < height; j++) {
        for (int i = 0; i < width; i++) {
            seen1[j][i] = false;
            seen9[j][i] = false;
        }
    }

    Point knots[10];
    for (int i = 0; i < 10; i++) {
        knots[i] = origin_point;
    }

    int unique_tail1_positions = 0;
    int unique_tail9_positions = 0;
    for (int i = 0; i < head_positions.length; i++) {
        knots[0] = points_get(head_positions, i);
        for (int j = 1; j < 10; j++) {
            Point p = next_tail_pos(knots[j - 1], knots[j]);
            int x = x_min + p.x;
            int y = y_min + p.y;
            if (j == 1 && !seen1[y][x]) {
                seen1[y][x] = true;
                unique_tail1_positions++;
            }
            if (j == 9 && !seen9[y][x]) {
                seen9[y][x] = true;
                unique_tail9_positions++;
            }
            knots[j] = p;
        }
    }

    printf("Part 1: %d\n", unique_tail1_positions);
    printf("Part 2: %d\n", unique_tail9_positions);

    END_TIMER();
    return 0;
}
