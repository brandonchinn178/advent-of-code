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

static int apply_operation(Operation op, int x) {
    switch (op.common.type) {
        case OP_ADD:
            return x + op.add.arg;
        case OP_MULT:
            return x * op.mult.arg;
        case OP_SQUARE:
            return x * x;
    }
}

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;
    size_t line_len;

    // hardcoded to number of monkeys in input
    size_t monkeys_length = 8;
    MonkeyInfo monkeys[monkeys_length];
    MonkeyItems all_monkeys_items[monkeys_length];
    int curr_monkey = 0;

    while ((line_len = get_line(&line, stdin)) != -1) {
        MonkeyInfo *monkey = &monkeys[curr_monkey];
        if (is_prefix(line, "Monkey")) {
            sscanf(line, "Monkey %d:", &curr_monkey);
            size_t initial_capacity = 10;
            int *items = malloc(initial_capacity * sizeof(int));
            all_monkeys_items[curr_monkey] = (MonkeyItems) {
                .items = items,
                .items_length = 0,
                .items_capacity = initial_capacity,
            };
        } else if (is_prefix(line, "  Starting items:")) {
            MonkeyItems *monkey_items = &all_monkeys_items[curr_monkey];
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

    // part 1
    int monkey_item_count[monkeys_length];
    for (int i = 0; i < monkeys_length; i++) {
        monkey_item_count[i] = 0;
    }
    for (int i = 0; i < 20; i++) {
        for (int monkey_idx = 0; monkey_idx < num_monkeys; monkey_idx++) {
            MonkeyInfo *monkey = &monkeys[monkey_idx];
            MonkeyItems *monkey_items = &all_monkeys_items[monkey_idx];
            for (int item_idx = 0; item_idx < monkey_items->items_length; item_idx++) {
                monkey_item_count[monkey_idx]++;
                int item = monkey_items->items[item_idx];
                item = apply_operation(monkey->operation, item);
                item /= 3;
                int next_monkey;
                if (item % monkey->test_modulus == 0) {
                    next_monkey = monkey->test_monkey_true;
                } else {
                    next_monkey = monkey->test_monkey_false;
                }
                add_item(&all_monkeys_items[next_monkey], item);
            }
            monkey_items->items_length = 0;
        }
    }

    sort_list_inplace(monkey_item_count, num_monkeys, DESC);
    printf("Part 1: %d\n", monkey_item_count[0] * monkey_item_count[1]);

    END_TIMER();
    return 0;
}
