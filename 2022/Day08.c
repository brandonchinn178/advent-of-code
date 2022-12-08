#include "./utils.h"

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;
    size_t line_len;

    // grid[y][x] gets the tree at the (x, y) coordinate,
    // where (0, 0) is the top-left corner
    int **grid = NULL;
    int width, height;

    int line_num = 0;
    while ((line_len = get_line(&line, stdin)) != -1) {
        if (grid == NULL) {
            // assume square
            width = line_len;
            height = line_len;
            grid = malloc(height * sizeof(int*));
            for (int y = 0; y < height; y++) {
                grid[y] = malloc(width * sizeof(int));
            }
        }

        for (int x = 0; x < width; x++) {
            grid[line_num][x] = line[x] - '0';
        }

        line_num++;
    }

    int num_visible = 0;
    int high_score = 0;

    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            int tree = grid[y][x];

            int num_visible_left = 0;
            bool visible_from_left = true;
            for (int i = x - 1; i >= 0; i--) {
                num_visible_left++;
                if (grid[y][i] >= tree) {
                    visible_from_left = false;
                    break;
                }
            }

            int num_visible_right = 0;
            bool visible_from_right = true;
            for (int i = x + 1; i < width; i++) {
                num_visible_right++;
                if (grid[y][i] >= tree) {
                    visible_from_right = false;
                    break;
                }
            }

            int num_visible_top = 0;
            bool visible_from_top = true;
            for (int j = y - 1; j >= 0; j--) {
                num_visible_top++;
                if (grid[j][x] >= tree) {
                    visible_from_top = false;
                    break;
                }
            }

            int num_visible_bottom = 0;
            bool visible_from_bottom = true;
            for (int j = y + 1; j < height; j++) {
                num_visible_bottom++;
                if (grid[j][x] >= tree) {
                    visible_from_bottom = false;
                    break;
                }
            }

            // part 1
            if (
                visible_from_left ||
                visible_from_right ||
                visible_from_top ||
                visible_from_bottom
            ) {
                num_visible++;
            }

            // part 2
            int score = num_visible_left * num_visible_right * num_visible_top * num_visible_bottom;
            if (score > high_score) {
                high_score = score;
            }
        }
    }
    printf("Part 1: %d\n", num_visible);
    printf("Part 2: %d\n", high_score);

    END_TIMER();
    return 0;
}
