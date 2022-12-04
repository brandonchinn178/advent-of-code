#include "./utils.h"

int main(int argc, char **argv) {
    START_TIMER();

    char* line = NULL;

    int part1_total = 0;
    int part2_total = 0;

    while (get_line(&line, stdin) != -1) {
        int elf1_start, elf1_end, elf2_start, elf2_end;
        sscanf(line, "%d-%d,%d-%d", &elf1_start, &elf1_end, &elf2_start, &elf2_end);
        if (
            (elf1_start <= elf2_start && elf2_end <= elf1_end)
            || (elf2_start <= elf1_start && elf1_end <= elf2_end)
        ) {
            part1_total++;
        }
        if (elf1_start <= elf2_end && elf1_end >= elf2_start) {
            part2_total++;
        }
    }

    printf("Part 1: %d\n", part1_total);
    printf("Part 2: %d\n", part2_total);

    END_TIMER();
    return 0;
}
