#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_INTERVALS 35
#define MAX_DIGITS 10

typedef unsigned long long int BigInt;

typedef BigInt Interval[2];

typedef struct {
    Interval data[MAX_INTERVALS];
    int len;
} IntervalList;

bool is_digit(char c) {
    return '0' <= c && c <= '9';
}

void load_input(IntervalList *intervals) {
    char* line = NULL;
    size_t line_size = 0;
    int line_len = getline(&line, &line_size, stdin);

    int i = 0;
    while (i < line_len) {
        int interval_idx = intervals->len++;
        Interval *interval = &intervals->data[interval_idx];

        BigInt v1 = 0;
        while (is_digit(line[i])) {
            v1 = v1 * 10 + (line[i] - '0');
            i++;
        }

        i++; // "-"


        BigInt v2 = 0;
        while (i < line_len && is_digit(line[i])) {
            v2 = v2 * 10 + (line[i] - '0');
            i++;
        }

        i++; // "," or "\n" or EOF

        (*interval)[0] = v1;
        (*interval)[1] = v2;
    }
}

BigInt get_total(IntervalList *intervals, bool (*is_invalid)(BigInt)) {
    BigInt total = 0;
    for (int i = 0; i < intervals->len; i++) {
        BigInt start = intervals->data[i][0];
        BigInt end = intervals->data[i][1];
        for (BigInt j = start; j <= end; j++) {
            if (is_invalid(j)) {
                total += j;
            }
        }
    }
    return total;
}

int get_num_digits(BigInt n) {
    return (int)log10(n) + 1;
}

/* get_digit(1234, 0) == 4
 * get_digit(1234, 1) == 3
 * get_digit(1234, 2) == 2
 * get_digit(1234, 3) == 1
 */
int get_digit(BigInt n, int digit) {
    return n / (BigInt)pow(10, digit) % 10;
}

bool has_cycle(BigInt n, int digits, int size) {
    for (int i = 0; i < size; i++) {
        int digit = get_digit(n, i);
        for (int j = i; j < digits; j += size) {
            if (get_digit(n, j) != digit) {
                return false;
            }
        }
    }
    return true;
}

bool is_invalid_pt1(BigInt n) {
    int digits = get_num_digits(n);
    if (digits % 2 != 0) {
        return false;
    }
    return has_cycle(n, digits, digits / 2);
}

bool is_invalid_pt2(BigInt n) {
    int digits = get_num_digits(n);
    for (int i = 1; i < digits; i++) {
        if (digits % i == 0 && has_cycle(n, digits, i)) {
            return true;
        }
    }
    return false;
}

int main(int argc, char **argv) {
    IntervalList intervals = {.len = 0};
    load_input(&intervals);

    BigInt part1_total = get_total(&intervals, is_invalid_pt1);
    printf("Part 1: %llu\n", part1_total);

    BigInt part2_total = get_total(&intervals, is_invalid_pt2);
    printf("Part 2: %llu\n", part2_total);

    return 0;
}
