#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef char *Result;
#define ERR(msg) msg
#define OK NULL
#define CHECK(e)                                                               \
  do {                                                                         \
    Result r = e;                                                              \
    if (r != OK) {                                                             \
      return r;                                                                \
    }                                                                          \
  } while (0)

/***** Types *****/

#define MAX_COLS 141
#define MAX_ROWS 142

typedef unsigned long long int BigInt;

// Maps row to col of a splitter on that row.
//
// Splitters at (1, 2) and (3, 2) will be represented as
//   splitterMap[2][0] = 1
//   splitterMap[2][1] = 3
typedef int SplitterMap[MAX_ROWS][MAX_COLS];

/***** Parsing *****/

void process_line(int *start_col, SplitterMap splitters,
                  size_t row_splitter_count[], size_t *num_rows, char *line,
                  size_t line_len, size_t line_num) {
  for (int i = 0; i < line_len; i++) {
    switch (line[i]) {
    case 'S':
      *start_col = i;
      break;
    case '^':
      splitters[line_num][(row_splitter_count[line_num])++] = i;
      break;
    }
  }

  (*num_rows)++;
}

void load_input(int *start_col, SplitterMap splitters,
                size_t row_splitter_count[], size_t *num_rows) {
  char *line = NULL;
  size_t line_size = 0;
  for (size_t line_num = 0;; line_num++) {
    size_t line_len = getline(&line, &line_size, stdin);
    if (line_len == -1 || line_len == 0) {
      break;
    }
    if (line[line_len - 1] == '\n') {
      line_len--;
    }

    process_line(start_col, splitters, row_splitter_count, num_rows, line, line_len,
                 line_num);
  }
}

/***** Entrypoint *****/

bool splitter_exists(SplitterMap splitters, size_t row_splitter_count[],
                     int row, int col) {
  for (int i = 0; i < row_splitter_count[row]; i++) {
    if (splitters[row][i] == col) {
      return true;
    }
  }
  return false;
}

Result run() {
  int start_col;
  SplitterMap splitters;
  size_t row_splitter_count[MAX_ROWS] = {0};
  size_t num_rows = 0;
  load_input(&start_col, splitters, row_splitter_count, &num_rows);

  size_t splitter_count = 0;
  BigInt beam_counts[MAX_COLS] = {0};
  beam_counts[start_col] = 1;
  for (int row = 0; row < num_rows; row++) {
    BigInt next_beam_counts[MAX_COLS] = {0};
    for (int col = 0; col < MAX_COLS; col++) {
      BigInt beam_count = beam_counts[col];
      if (beam_count > 0) {
        if (splitter_exists(splitters, row_splitter_count, row, col)) {
          next_beam_counts[col - 1] += beam_count;
          next_beam_counts[col + 1] += beam_count;
          splitter_count++;
        } else {
          next_beam_counts[col] += beam_count;
        }
      }
    }

    // Copy beam counts over
    for (int i = 0; i < MAX_COLS; i++) {
      // if (next_beam_counts[i] > 0) printf("Beam count: (%d, %d) - %d\n", i, row, next_beam_counts[i]);
      beam_counts[i] = next_beam_counts[i];
    }
  }

  printf("Part 1: %zu\n", splitter_count);

  BigInt total = 0;
  for (int i = 0; i < MAX_COLS; i++) {
    total += beam_counts[i];
  }
  printf("Part 2: %llu\n", total);

  return OK;
}

int main(int argc, char **argv) {
  Result r = run();
  if (r != OK) {
    printf("ERROR: %s\n", r);
    return 1;
  }
  return 0;
}
