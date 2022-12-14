#include "./utils.h"

int main(int argc, char **argv) {
    START_TIMER();

    char* line = NULL;
    size_t line_len = 0;

    // {#1 elf, #2 elf, #3 elf, current elf}
    int calories[4] = {0};

    while (line_len != -1) {
        line_len = get_line(&line, stdin);
        if (line_len == -1 || line_len == 0) {
            sort_int_list_inplace(calories, 4, DESC);
            calories[3] = 0;
        } else {
            calories[3] += atoi(line);
        }
    }

    // part 1
    printf("Most calories: %d\n", calories[0]);

    // part 2
    printf(
        "Sum of top 3 calories: %d\n",
        calories[0] + calories[1] + calories[2]
    );

    END_TIMER();
    return 0;
}
