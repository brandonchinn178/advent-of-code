#include "./utils.h"

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;

    int cycle = 1;
    int val = 1;
    int part1_total = 0;
    bool display[6][40];

    while (get_line(&line, stdin) != -1) {
        int cycles, delta;
        if (strcmp(line, "noop") == 0) {
            cycles = 1;
            delta = 0;
        } else {
            cycles = 2;
            sscanf(line, "addx %d", &delta);
        }
        for (int i = 0; i < cycles; i++) {
            // part 1
            if (cycle % 40 == 20) {
                part1_total += cycle * val;
            }

            // part 2
            int pixel_y = (cycle - 1) / 40;
            int pixel_x = (cycle - 1) % 40;
            display[pixel_y][pixel_x] = val - 1 <= pixel_x && pixel_x <= val + 1;

            cycle++;
        }
        val += delta;
    }

    printf("Part 1: %d\n", part1_total);
    printf("Part 2:\n");
    for (int j = 0; j < 6; j++) {
        for (int i = 0; i < 40; i++) {
            printf(display[j][i] ? "█" : " ");
        }
        printf("\n");
    }

    END_TIMER();
    return 0;
}
