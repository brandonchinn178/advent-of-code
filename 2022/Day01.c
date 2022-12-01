#include <string.h>

#include "./utils.h"

int main(int argc, char **argv) {
    char* input = freadall("data/Day01.txt");
    Lines lines = split_lines(input);

    int* calories = NULL;
    int num_elves = 0;
    int curr_elf_calories = 0;
    for (int i = 0; i < lines.length; i++) {
        char* line = get_line(lines, i);
        if (strlen(line) == 0) {
            if (curr_elf_calories > 0) {
                calories = realloc(calories, (num_elves + 1) * sizeof(int));
                calories[num_elves] = curr_elf_calories;
                num_elves++;
            }
            curr_elf_calories = 0;
        } else {
            curr_elf_calories += atoi(line);
        }
        free(line);
    }

    sort_list_inplace(calories, num_elves);

    int calories_first = calories[num_elves - 1];
    int calories_second = calories[num_elves - 2];
    int calories_third = calories[num_elves - 3];

    // part 1
    printf("Most calories: %d\n", calories_first);

    // part 2
    printf(
        "Sum of top 3 calories: %d\n",
        calories_first + calories_second + calories_third
    );

    free_lines(lines);
    free(input);
    return 0;
}
