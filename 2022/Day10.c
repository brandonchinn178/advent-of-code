#include "./utils.h"

typedef struct {
    int vals[6];
} Signals;

#define EMPTY_SIGNALS ((Signals) { .vals = {0} })

static int to_signal_index(int cycle) {
    switch (cycle) {
        case 20: return 0;
        case 60: return 1;
        case 100: return 2;
        case 140: return 3;
        case 180: return 4;
        case 220: return 5;
        default: return -1;
    }
}

static void save_signal(Signals *signals, int cycle, int val) {
    int index = to_signal_index(cycle);
    if (index < 0) return;
    signals->vals[index] = val;
}

static int get_signal_strength(Signals *signals, int cycle) {
    int index = to_signal_index(cycle);
    return signals->vals[index] * cycle;
}

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;

    Signals signals = EMPTY_SIGNALS;

    int cycle = 1;
    int val = 1;

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
            save_signal(&signals, cycle, val);

            // part 2
            int pixel_pos = (cycle - 1) % 40;
            printf(
                (val - 1 <= pixel_pos && pixel_pos <= val + 1)
                    ? "#"
                    : "."
            );
            if (cycle % 40 == 0) printf("\n");

            cycle++;
        }
        val += delta;
    }

    int part1_total =
        get_signal_strength(&signals, 20) +
        get_signal_strength(&signals, 60) +
        get_signal_strength(&signals, 100) +
        get_signal_strength(&signals, 140) +
        get_signal_strength(&signals, 180) +
        get_signal_strength(&signals, 220);
    printf("Part 1: %d\n", part1_total);

    END_TIMER();
    return 0;
}
