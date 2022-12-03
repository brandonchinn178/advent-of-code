#include "./utils.h"

typedef uint8_t Priority;

Priority to_priority(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 1;
    }
    if (c >= 'A' && c <= 'Z') {
        return c - 'A' + 27;
    }
    ABORT("Invalid char: %c", c);
}

// Bit N (zero-indexed) is 1 when item with priority (N + 1) is in the set.
typedef uint64_t Items;
#define EMPTY_ITEMS 0

static void insert_item(Items* items, Priority p) {
    uint64_t mask = 1UL << (p - 1);
    *items |= mask;
}

static bool contains_item(Items items, Priority p) {
    uint64_t mask = 1UL << (p - 1);
    return (items & mask) == mask;
}

static Items get_intersection(Items items1, Items items2) {
    return items1 & items2;
}

static Priority get_first_item(Items items) {
    for (Priority p = 1; p <= 52; p++) {
        if (contains_item(items, p)) {
            return p;
        }
    }
    ABORT("Items was empty");
}

int main(int argc, char **argv) {
    START_TIMER();

    char* line = NULL;
    size_t line_len;
    uint32_t line_num = 0;

    int part1_total = 0;

    Items group = EMPTY_ITEMS;
    int part2_total = 0;

    while ((line_len = get_line(&line, stdin)) != -1) {
        Items sack = EMPTY_ITEMS;
        Items compartment1 = EMPTY_ITEMS;
        Items compartment2 = EMPTY_ITEMS;

        for (int i = 0; i < line_len; i++) {
            Priority p = to_priority(line[i]);
            insert_item(&sack, p);
            insert_item(i < line_len / 2 ? &compartment1 : &compartment2, p);
        }

        part1_total += get_first_item(get_intersection(compartment1, compartment2));

        switch (line_num % 3) {
            case 0:
                group = sack;
                break;
            case 1:
                group = get_intersection(group, sack);
                break;
            case 2:
                group = get_intersection(group, sack);
                part2_total += get_first_item(group);
                break;
        }

        line_num++;
    }

    printf("Part 1: %d\n", part1_total);
    printf("Part 2: %d\n", part2_total);

    END_TIMER();
    return 0;
}
