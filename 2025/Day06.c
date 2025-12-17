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

#define MAX_COLS 3800
#define MAX_ROWS 6
#define MAX_GROUPS 1000

typedef unsigned long long int BigInt;

typedef char Grid[MAX_ROWS][MAX_COLS];

typedef enum {
  ADD,
  MUL,
} Op;

typedef struct {
  Op op;
  BigInt total;
} GroupAcc;

GroupAcc GroupAcc_new(Op op) {
  BigInt total;
  switch (op) {
  case ADD:
    total = 0;
    break;
  case MUL:
    total = 1;
    break;
  }
  return (GroupAcc){.op = op, .total = total};
}

void GroupAcc_update(GroupAcc *acc, int num) {
  switch (acc->op) {
  case ADD:
    acc->total += num;
    break;
  case MUL:
    acc->total *= num;
    break;
  }
}

typedef struct {
  int value;
} NumParser;

NumParser NumParser_New() {
  return (NumParser){.value = 0};
}

void NumParser_read(NumParser *parser, char c) {
  if ('0' <= c && c <= '9') {
    parser->value = parser->value * 10 + (c - '0');
  }
}

/***** Parsing *****/

void process_line(Grid grid, size_t *num_rows, size_t *num_cols, char *line,
                  size_t line_len, size_t line_num) {
  for (int i = 0; i < line_len; i++) {
    grid[line_num][i] = line[i];
  }

  (*num_rows)++;
  if (*num_cols == 0) {
    *num_cols = line_len;
  }
}

void parse_ops(Op ops[], size_t *num_groups, char row[], size_t num_cols) {
  for (int i = 0; i < num_cols; i++) {
    Op op;
    switch (row[i]) {
    case '+':
      op = ADD;
      break;
    case '*':
      op = MUL;
      break;
    default:
      continue;
    }
    ops[(*num_groups)++] = op;
  }
}

void load_input(Grid grid, size_t *num_rows, size_t *num_cols) {
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

    process_line(grid, num_rows, num_cols, line, line_len, line_num);
  }
}

/***** Entrypoint *****/

void detect_borders(size_t borders[], Grid grid, size_t num_rows,
                    size_t num_cols) {
  size_t group = 0;

  for (int j = 0; j < num_cols; j++) {
    bool is_border = true;
    for (int i = 0; i < num_rows; i++) {
      if (grid[i][j] != ' ') {
        is_border = false;
        break;
      }
    }
    if (is_border) {
      borders[group++] = j;
    }
  }
}

Result run() {
  Grid grid;
  size_t num_rows = 0;
  size_t num_cols = 0;
  load_input(grid, &num_rows, &num_cols);

  Op ops[MAX_GROUPS];
  size_t num_groups = 0;
  parse_ops(ops, &num_groups, grid[num_rows - 1], num_cols);
  num_rows--;

  size_t borders[MAX_GROUPS];
  detect_borders(borders, grid, num_rows, num_cols);
  borders[num_groups - 1] = num_cols;

  BigInt total1 = 0, total2 = 0;
  for (int i = 0; i < num_groups; i++) {
    Op op = ops[i];
    int start = i == 0 ? 0 : borders[i - 1] + 1;
    int end = borders[i];
    int width = end - start;

    GroupAcc acc1 = GroupAcc_new(op);
    GroupAcc acc2 = GroupAcc_new(op);

    int digits1 = width;
    int digits2 = num_rows;
    int iters = digits1 * digits2;

    NumParser num1, num2;
    for (int j = 0; j < iters; j++) {
      if (j % digits1 == 0) {
        num1 = NumParser_New();
      }
      if (j % digits2 == 0) {
        num2 = NumParser_New();
      }

      char c1 = grid[j / digits1][start + (j % digits1)];
      char c2 = grid[j % digits2][start + (j / digits2)];

      NumParser_read(&num1, c1);
      NumParser_read(&num2, c2);

      if ((j + 1) % digits1 == 0) {
        GroupAcc_update(&acc1, num1.value);
      }
      if ((j + 1) % digits2 == 0) {
        GroupAcc_update(&acc2, num2.value);
      }
    }

    total1 += acc1.total;
    total2 += acc2.total;
  }

  printf("Part 1: %llu\n", total1);
  printf("Part 2: %llu\n", total2);

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
