#include "./utils.h"

// Stack consists of a heap-allocated array of
// characters, the size of the array, and the
// current number of elements in the array,
// where contents[length - 1] is the top of the
// stack (for length > 0) and contents[0] is
// the bottom of the stack
typedef struct {
    char* contents;
    size_t contents_size;
    size_t length;
} Stack;

// The representation of a Stack in the parsing phase,
// where the stack is reversed (contents[0] is the
// top of the stack and contents[length - 1] is the last
// element in the stack parsed so far)
typedef Stack StackBuilder;

// Crates represented as a heap-allocated array of Stacks
typedef struct {
    Stack* stacks;
    size_t num_stacks;
} Crates;

/***** Utilities *****/

static char from_top_of_stack(Stack stack, size_t n) {
    size_t i = stack.length - n - 1;
    return i < 0 ? ' ' : stack.contents[i];
}

static void print_crates(Crates crates) {
    for (int i = 0; i < crates.num_stacks; i++) {
        Stack stack = crates.stacks[i];
        printf("| Stack %d: (bottom) ", i + 1);
        for (int j = 0; j < stack.length; j++) {
            printf("%c, ", stack.contents[j]);
        }
        printf("(top)\n");
    }
}

static void print_top_of_stacks(Crates crates) {
    for (int i = 0; i < crates.num_stacks; i++) {
        Stack stack = crates.stacks[i];
        printf("%c", from_top_of_stack(stack, 0));
    }
    printf("\n");
}

static Crates copy_crates(Crates crates) {
    Stack* stacks = malloc(crates.num_stacks * sizeof(Stack));
    for (int i = 0; i < crates.num_stacks; i++) {
        Stack stack = crates.stacks[i];

        char* contents = malloc(stack.contents_size * sizeof(char));
        for (int j = 0; j < stack.length; j++) {
            contents[j] = stack.contents[j];
        }

        stacks[i] = (Stack) {
            .contents = contents,
            .contents_size = stack.contents_size,
            .length = stack.length,
        };
    }

    return (Crates) {
        .stacks = stacks,
        .num_stacks = crates.num_stacks,
    };
}

static void resize_stack_if_necessary(Stack* stack, size_t num_to_add) {
    if (stack->length + num_to_add > stack->contents_size) {
        size_t new_size = (stack->contents_size *= 2);
        stack->contents = realloc(stack->contents, new_size * sizeof(char));
    }
}

/***** Parsing *****/

static StackBuilder init_stack() {
    // take a guess for initial size, will be resized as necessary later
    size_t initial_stack_size = 10;
    char* contents = malloc(initial_stack_size * sizeof(char));
    return (StackBuilder) {
        .contents = contents,
        .contents_size = initial_stack_size,
        .length = 0,
    };
}

static Crates EMPTY_CRATES = (Crates) {
    .stacks = NULL,
    .num_stacks = 0,
};

static Crates init_crates(size_t num_stacks) {
    StackBuilder* stacks = malloc(num_stacks * sizeof(StackBuilder));
    for (int i = 0; i < num_stacks; i++) {
        stacks[i] = init_stack();
    }

    return (Crates) {
        .stacks = stacks,
        .num_stacks = num_stacks,
    };
}

static void add_to_stack(StackBuilder* stack, char c) {
    resize_stack_if_necessary(stack, 1);
    stack->contents[stack->length] = c;
    stack->length++;
}

static void parse_stacks(Crates* crates, char* line, size_t line_len) {
    size_t num_stacks = (line_len + 1) / 4;
    if (crates->stacks == NULL) {
        *crates = init_crates(num_stacks);
    }

    for (int i = 0; i < num_stacks; i++) {
        char c = line[4 * i + 1];
        if (c != ' ') {
            add_to_stack(&crates->stacks[i], c);
        }
    }
}

static void finalize_stack(StackBuilder stack) {
    size_t len = stack.length;
    for (int i = 0; i < len / 2; i++) {
        char tmp = stack.contents[i];
        stack.contents[i] = stack.contents[len - i - 1];
        stack.contents[len - i - 1] = tmp;
    }
}

static void finalize_crates(Crates* crates) {
    for (int i = 0; i < crates->num_stacks; i++) {
        finalize_stack(crates->stacks[i]);
    }
}

/***** Entrypoint *****/

int main(int argc, char **argv) {
    START_TIMER();

    char* line = NULL;
    size_t line_len;

    bool parsing_initial = true;
    Crates crates1 = EMPTY_CRATES;
    Crates crates2 = EMPTY_CRATES;

    while ((line_len = get_line(&line, stdin)) != -1) {
        if (line_len == 0) {
            parsing_initial = false;
            finalize_crates(&crates1);
            crates2 = copy_crates(crates1);
            // printf("===== After parsing =====\n");
            // print_crates(crates1);
        } else if (parsing_initial) {
            // ignore the row of numbers
            if (is_prefix(line, " 1")) {
                continue;
            }
            parse_stacks(&crates1, line, line_len);
        } else {
            int count, start, end;
            sscanf(line, "move %d from %d to %d", &count, &start, &end);

            // move part 1
            Stack* from1 = &crates1.stacks[start - 1];
            Stack* to1 = &crates1.stacks[end - 1];
            resize_stack_if_necessary(to1, count);
            for (int i = 0; i < count; i++) {
                to1->contents[to1->length + i] = from_top_of_stack(*from1, i);
            }
            from1->length -= count;
            to1->length += count;
            // printf("===== After move %d -> %d (%d times) =====\n", start, end, count);
            // print_crates(crates1);

            // move part 2
            Stack* from2 = &crates2.stacks[start - 1];
            Stack* to2 = &crates2.stacks[end - 1];
            resize_stack_if_necessary(to2, count);
            for (int i = 0; i < count; i++) {
                to2->contents[to2->length + i] = from_top_of_stack(*from2, count - i - 1);
            }
            from2->length -= count;
            to2->length += count;
            // printf("===== After moving %d crates: %d -> %d =====\n", count, start, end);
            // print_crates(crates2);
        }
    }

    printf("Top of each stack, part 1: ");
    print_top_of_stacks(crates1);
    printf("Top of each stack, part 2: ");
    print_top_of_stacks(crates2);

    END_TIMER();
    return 0;
}
