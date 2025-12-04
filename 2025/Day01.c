#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_VALS 4500
typedef int Values[MAX_VALS];

void load_input(Values vals, int* num_vals) {
    char* line = NULL;
    size_t line_size = 0;
    int line_len = 0;
    int line_num = 0;
    while (true) {
        line_len = getline(&line, &line_size, stdin);
        if (line_len == -1 || line_len == 0) {
            break;
        }

        int val = atoi(line + 1);
        if (line[0] == 'L') {
            val *= -1;
        }
        vals[line_num] = val;

        line_num++;
    }
    *num_vals = line_num;
}

int mod(int n, int d) {
    return (n % d + d) % d;
}

int part1(int *pos, int val) {
    *pos = mod(*pos + val, 100);
    return *pos == 0 ? 1 : 0;
}

int part2(int *pos, int val) {
    int prev = *pos;
    int next = prev + val;
    *pos = mod(next, 100);
    return (
        (abs(next) / 100) +
        (prev > 0 && next < 0 ? 1 : 0) +
        (next == 0 ? 1 : 0)
    );
}

int main(int argc, char **argv) {
    Values vals;
    int num_vals;
    load_input(vals, &num_vals);

    int part1_total = 0;
    int part1_pos = 50;
    for (int i = 0; i < num_vals; i++) {
        part1_total += part1(&part1_pos, vals[i]);
    }
    printf("Part 1: %d\n", part1_total);

    int part2_total = 0;
    int part2_pos = 50;
    for (int i = 0; i < num_vals; i++) {
        part2_total += part2(&part2_pos, vals[i]);
    }
    printf("Part 2: %d\n", part2_total);

    return 0;
}
