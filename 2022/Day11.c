#include "./utils.h"

typedef enum {
    OP_ADD,
    OP_MULT,
    OP_SQUARE,
} OperationType;

typedef union {
    struct { OperationType type; } common;
    struct { OperationType type; int arg; } add;
    struct { OperationType type; int arg; } mult;
    struct { OperationType type; } square;
} Operation;

typedef struct {
    Operation operation;

    int test_modulus;
    int test_monkey_true;
    int test_monkey_false;
} MonkeyInfo;

typedef struct {
    int* items;
    size_t items_length;
    size_t items_capacity;
} MonkeyItems;

static void add_item(MonkeyItems *monkey, int item) {
    if (monkey->items_length == monkey->items_capacity) {
        monkey->items_capacity *= 2;
        monkey->items = realloc(monkey->items, monkey->items_capacity * sizeof(int));
    }
    monkey->items[monkey->items_length] = item;
    monkey->items_length++;
}

static MonkeyItems copy_items(MonkeyItems monkey_items) {
    size_t items_length = monkey_items.items_length;
    size_t items_bytes = items_length * sizeof(int);
    int *items = malloc(items_bytes);
    memcpy(items, monkey_items.items, items_bytes);
    return (MonkeyItems) {
        .items = items,
        .items_length = items_length,
        .items_capacity = items_length,
    };
}

static uint64_t apply_operation(Operation op, uint64_t x) {
    switch (op.common.type) {
        case OP_ADD:
            return x + op.add.arg;
        case OP_MULT:
            return x * op.mult.arg;
        case OP_SQUARE:
            return x * x;
    }
}

static void do_round(
    int num_monkeys,
    MonkeyInfo *monkeys,
    MonkeyItems *monkeys_items,
    int *monkey_counts,
    int relief_factor,
    int shared_modulus
) {
    for (int monkey_idx = 0; monkey_idx < num_monkeys; monkey_idx++) {
        MonkeyInfo *monkey = &monkeys[monkey_idx];
        MonkeyItems *monkey_items = &monkeys_items[monkey_idx];
        for (int item_idx = 0; item_idx < monkey_items->items_length; item_idx++) {
            monkey_counts[monkey_idx]++;
            uint64_t item = monkey_items->items[item_idx];
            item = apply_operation(monkey->operation, item);
            item /= relief_factor;
            int new_item = item % shared_modulus;
            int next_monkey;
            if (new_item % monkey->test_modulus == 0) {
                next_monkey = monkey->test_monkey_true;
            } else {
                next_monkey = monkey->test_monkey_false;
            }
            add_item(&monkeys_items[next_monkey], new_item);
        }
        monkey_items->items_length = 0;
    }
}

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;
    size_t line_len;

    // hardcoded to number of monkeys in input
    size_t monkeys_length = 8;
    MonkeyInfo monkeys[monkeys_length];
    MonkeyItems initial_items[monkeys_length];
    int curr_monkey = 0;

    while ((line_len = get_line(&line, stdin)) != -1) {
        MonkeyInfo *monkey = &monkeys[curr_monkey];
        if (is_prefix(line, "Monkey")) {
            sscanf(line, "Monkey %d:", &curr_monkey);
            size_t initial_capacity = 10;
            int *items = malloc(initial_capacity * sizeof(int));
            initial_items[curr_monkey] = (MonkeyItems) {
                .items = items,
                .items_length = 0,
                .items_capacity = initial_capacity,
            };
        } else if (is_prefix(line, "  Starting items:")) {
            MonkeyItems *monkey_items = &initial_items[curr_monkey];
            int i = 18;
            int curr_item = 0;
            while (i <= line_len) {
                char c = i < line_len ? line[i] : ',';
                if ('0' <= c && c <= '9') {
                    curr_item = curr_item * 10 + (c - '0');
                    i++;
                } else {
                    add_item(monkey_items, curr_item);
                    curr_item = 0;
                    i += 2;
                }
            }
        } else if (is_prefix(line, "  Operation:")) {
            char *op_raw = line + 19;
            if (strcmp(op_raw, "old * old") == 0) {
                monkey->operation = (Operation) {
                    .square = { .type = OP_SQUARE },
                };
            } else if (is_prefix(op_raw, "old *")) {
                int arg;
                sscanf(op_raw, "old * %d", &arg);
                monkey->operation = (Operation) {
                    .mult = { .type = OP_MULT, .arg = arg },
                };
            } else {
                int arg;
                sscanf(op_raw, "old + %d", &arg);
                monkey->operation = (Operation) {
                    .add = { .type = OP_ADD, .arg = arg },
                };
            }
        } else if (is_prefix(line, "  Test:")) {
            sscanf(line, "  Test: divisible by %d", &monkey->test_modulus);
        } else if (is_prefix(line, "    If true:")) {
            sscanf(line, "    If true: throw to monkey %d", &monkey->test_monkey_true);
        } else {
            sscanf(line, "    If false: throw to monkey %d", &monkey->test_monkey_false);
        }
    }
    int num_monkeys = curr_monkey + 1;

    int shared_modulus = 1;
    for (int i = 0; i < num_monkeys; i++) {
        shared_modulus *= monkeys[i].test_modulus;
    }

    // part 1
    MonkeyItems part1_items[num_monkeys];
    int part1_monkey_counts[num_monkeys];
    for (int i = 0; i < num_monkeys; i++) {
        part1_items[i] = copy_items(initial_items[i]);
        part1_monkey_counts[i] = 0;
    }
    for (int i = 0; i < 20; i++) {
        do_round(num_monkeys, monkeys, part1_items, part1_monkey_counts, 3, shared_modulus);
    }
    qsort(part1_monkey_counts, num_monkeys, sizeof(int), cmp_int_desc);
    printf("Part 1: %d\n", part1_monkey_counts[0] * part1_monkey_counts[1]);

    // part 2
    MonkeyItems part2_items[num_monkeys];
    int part2_monkey_counts[num_monkeys];
    for (int i = 0; i < num_monkeys; i++) {
        part2_items[i] = copy_items(initial_items[i]);
        part2_monkey_counts[i] = 0;
    }
    for (int i = 0; i < 10000; i++) {
        do_round(num_monkeys, monkeys, part2_items, part2_monkey_counts, 1, shared_modulus);
    }
    qsort(part2_monkey_counts, num_monkeys, sizeof(int), cmp_int_desc);
    printf("Part 2: %llu\n", (uint64_t) part2_monkey_counts[0] * part2_monkey_counts[1]);

    END_TIMER();
    return 0;
}
