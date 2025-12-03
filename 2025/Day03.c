#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_BANKS 200
#define MAX_COLS 100
typedef char BatteryBank[MAX_COLS];
typedef BatteryBank BatteryBanks[MAX_BANKS];
typedef unsigned long long int Joltage;

void load_batteries(BatteryBanks batteries, int* num_banks, int* num_cols) {
    char* line = NULL;
    size_t line_size = 0;
    int line_len = 0;
    int bank = 0;
    while (true) {
        line_len = getline(&line, &line_size, stdin);
        if (line_len == -1 || line_len == 0) {
            break;
        }
        *num_cols = line_len - 1;
        for (int col = 0; col < *num_cols; col++) {
            batteries[bank][col] = line[col];
        }
        bank++;
    }
    *num_banks = bank;
}

Joltage get_joltage(BatteryBank bank, int num_cols, int num_digits) {
    Joltage acc = 0;

    for (; num_digits > 0; num_digits--) {
        int max_digit_pos = -1;
        for (int i = 0; i < num_cols - num_digits + 1; i++) {
            if (max_digit_pos == -1 || bank[i] > bank[max_digit_pos]) {
                max_digit_pos = i;
            }
        }

        int max_digit = bank[max_digit_pos] - '0';
        bank += max_digit_pos + 1;
        num_cols -= max_digit_pos + 1;
        acc = acc * 10 + max_digit;
    }

    return acc;
}

int main(int argc, char **argv) {
    BatteryBanks batteries;
    int num_banks, num_cols;
    load_batteries(batteries, &num_banks, &num_cols);

    Joltage part1_total = 0;
    for (int i = 0; i < num_banks; i++) {
        part1_total += get_joltage(batteries[i], num_cols, 2);
    }
    printf("Part 1: %llu\n", part1_total);

    Joltage part2_total = 0;
    for (int i = 0; i < num_banks; i++) {
        part2_total += get_joltage(batteries[i], num_cols, 12);
    }
    printf("Part 2: %llu\n", part2_total);

    return 0;
}
