#include "./utils.h"

int main(int argc, char **argv) {
    char* line = NULL;
    size_t size = 0;
    size_t line_len = 0;

    // {#1 elf, #2 elf, #3 elf, current elf}
    int calories[4] = {0};

    while (line_len != -1) {
        line_len = getline(&line, &size, stdin);
        if (line_len == -1 || line[0] == '\n') {
            sort_list_inplace(calories, 4, DESC);
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

    return 0;
}
