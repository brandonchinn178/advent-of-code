#include "./utils.h"

static bool is_unique(char* s, size_t start, size_t count) {
    for (size_t i = start; i < start + count; i++) {
        for (size_t j = i + 1; j < start + count; j++) {
            if (s[i] == s[j]) {
                return false;
            }
        }
    }
    return true;
}

int main(int argc, char **argv) {
    START_TIMER();

    char* line = NULL;
    size_t line_len = get_line(&line, stdin);
    bool found_part1 = false;

    // assume it exists, so not bothering to check out-of-bounds
    for (size_t start = 0; start < line_len; start++) {
        // part 1
        if (!found_part1) {
            if (is_unique(line, start, 4)) {
                printf("Part 1: %ld\n", start + 4);
                found_part1 = true;
            }
        }

        // part 2
        if (is_unique(line, start, 14)) {
            printf("Part 2: %ld\n", start + 14);
            break;
        }
    }

    END_TIMER();
    return 0;
}
