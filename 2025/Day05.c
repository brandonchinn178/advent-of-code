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

#define MAX_INTERVALS 177
#define MAX_IDS 1000

typedef unsigned long long int BigInt;
typedef BigInt Id;

typedef struct {
  Id lo;
  Id hi;
} Interval;

int _compare_interval(Interval a, Interval b) {
  if (a.lo < b.lo) {
    return -1;
  } else if (a.lo > b.lo) {
    return 1;
  }
  if (a.hi < b.hi) {
    return -1;
  } else if (a.hi > b.hi) {
    return 1;
  }
  return 0;
}

void _swap_intervals(Interval *a, Interval *b) {
  Interval t = *a;
  *a = *b;
  *b = t;
}

void _quick_sort_intervals(Interval intervals[], int start, int end) {
  if (start >= end) {
    return;
  }

  Interval pivot = intervals[end];
  int numLT = 0; // number of elements smaller than pivot
  for (int i = start; i < end; i++) {
    if (_compare_interval(intervals[i], pivot) < 0) {
      _swap_intervals(&intervals[i], &intervals[start + numLT]);
      numLT++;
    }
  }
  // Swap pivot into position
  _swap_intervals(&intervals[start + numLT], &intervals[end]);

  _quick_sort_intervals(intervals, start, start + numLT - 1);
  _quick_sort_intervals(intervals, start + numLT + 1, end);
}

void sort_intervals(Interval intervals[], size_t num_intervals) {
  return _quick_sort_intervals(intervals, 0, num_intervals - 1);
}

bool intervals_contain(Interval intervals[], size_t num_intervals, Id id) {
  for (int i = 0; i < num_intervals; i++) {
    Interval interval = intervals[i];
    if (interval.lo > id) {
      break;
    } else if (id <= interval.hi) {
      return true;
    }
  }
  return false;
}

BigInt intervals_cardinality(Interval intervals[], size_t num_intervals) {
  BigInt count = 0;
  BigInt start = 0;
  for (int i = 0; i < num_intervals; i++) {
    Interval interval = intervals[i];
    if (start >= interval.hi) {
      count += 0;
    } else if (start >= interval.lo) {
      count += interval.hi - start;
    } else {
      count += interval.hi - interval.lo + 1;
    }
    if (start < interval.hi) {
      start = interval.hi;
    }
  }
  return count;
}

/***** Parsing *****/

void process_line(Interval intervals[], size_t *num_intervals, Id ids[],
                  size_t *num_ids, bool *is_interval, char *line,
                  size_t line_len, size_t line_num) {
  if (line_len == 0) {
    *is_interval = false;
    return;
  }

  if (*is_interval) {
    int i = 0;

    BigInt lo = 0;
    while (line[i] != '-') {
      lo = lo * 10 + (line[i] - '0');
      i++;
    }

    i++;

    BigInt hi = 0;
    while (i < line_len) {
      hi = hi * 10 + (line[i] - '0');
      i++;
    }

    intervals[(*num_intervals)++] = (Interval){.lo = lo, .hi = hi};
  } else {
    BigInt id = 0;
    for (int i = 0; i < line_len; i++) {
      id = id * 10 + (line[i] - '0');
    }
    ids[(*num_ids)++] = id;
  }
}

void load_input(Interval intervals[], size_t *num_intervals, Id ids[],
                size_t *num_ids) {
  bool is_interval = true;

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

    process_line(intervals, num_intervals, ids, num_ids, &is_interval, line,
                 line_len, line_num);
  }
}

/***** Entrypoint *****/

Result run() {
  Interval intervals[MAX_INTERVALS];
  size_t num_intervals = 0;

  Id ids[MAX_IDS];
  size_t num_ids = 0;
  load_input(intervals, &num_intervals, ids, &num_ids);

  sort_intervals(intervals, num_intervals);

  int total = 0;
  for (int i = 0; i < num_ids; i++) {
    if (intervals_contain(intervals, num_intervals, ids[i])) {
      total++;
    }
  }
  printf("Part 1: %d\n", total);

  printf("Part 2: %llu\n", intervals_cardinality(intervals, num_intervals));

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
