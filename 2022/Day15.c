#include "./utils.h"

// #define TARGET_Y 10
#define TARGET_Y 2000000

typedef struct { int lo; int hi; } Range;

static int manhattan_distance(int x1, int y1, int x2, int y2) {
    return abs(y2 - y1) + abs(x2 - x1);
}

static Range* range_excluded(int x, int y, int beacon_x, int beacon_y, int target_y) {
    int dist_to_target = abs(y - target_y);
    int dist_to_beacon = manhattan_distance(x, y, beacon_x, beacon_y);
    if (dist_to_target > dist_to_beacon) {
        return NULL;
    }

    int diff = dist_to_beacon - dist_to_target;

    Range *range = malloc(sizeof(Range));
    *range = (Range) { .lo = x - diff, .hi = x + diff };
    return range;
}

static bool range_starts_before(Range *r1, Range *r2) {
    return r1->lo < r2->lo;
}

int main(int argc, char **argv) {
    START_TIMER();

    List part1_exclude_ranges = list_empty();

    char *line = NULL;
    while (get_line(&line, stdin) != -1) {
        int x, y, beacon_x, beacon_y;
        sscanf(
            line,
            "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
            &x,
            &y,
            &beacon_x,
            &beacon_y
        );
        Range *range = range_excluded(x, y, beacon_x, beacon_y, TARGET_Y);
        if (range != NULL) {
            int lo = range->lo, hi = range->hi;
            if (beacon_y == TARGET_Y && lo <= beacon_x && beacon_x <= hi) {
                range->hi = beacon_x - 1;
                Range *range2 = malloc(sizeof(Range));
                range2->lo = beacon_x + 1;
                range2->hi = hi;
                list_append(&part1_exclude_ranges, range);
                list_append(&part1_exclude_ranges, range2);
            } else {
                list_append(&part1_exclude_ranges, range);
            }
        }
    }

    sort_list_inplace(&part1_exclude_ranges, ASC, (IsLessThan) range_starts_before);

    int part1_count = 0;
    int prev;
    for (int i = 0; i < part1_exclude_ranges.length; i++) {
        Range *range = list_get(part1_exclude_ranges, i);
        int lo = range->lo, hi = range->hi;
        if (i > 0) {
            lo = max(prev + 1, lo);
            hi = max(prev, hi);
        }
        part1_count += hi - lo + 1;
        prev = hi;
    }

    printf("Part 1 (y = %d): %d\n", TARGET_Y, part1_count);

    END_TIMER();
    return 0;
}
