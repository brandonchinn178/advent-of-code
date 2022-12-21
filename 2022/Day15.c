#include "./utils.h"

// #define TARGET_Y 10
// #define MAX_COORD 20
#define TARGET_Y 2000000
#define MAX_COORD 4000000

typedef struct { int lo, hi; } Range;

typedef struct { int x, y; } Beacon;
typedef struct {
    int x, y;
    Beacon beacon;
} Sensor;

static int manhattan_distance(int x1, int y1, int x2, int y2) {
    return abs(y2 - y1) + abs(x2 - x1);
}

static Range* range_excluded(Sensor *sensor, int target_y) {
    int dist_to_target = abs(sensor->y - target_y);
    int dist_to_beacon = manhattan_distance(
        sensor->x,
        sensor->y,
        sensor->beacon.x,
        sensor->beacon.y
    );

    if (dist_to_target > dist_to_beacon) {
        return NULL;
    }

    int diff = dist_to_beacon - dist_to_target;

    Range *range = malloc(sizeof(Range));
    *range = (Range) { .lo = sensor->x - diff, .hi = sensor->x + diff };
    return range;
}

static int cmp_range(const void *r1, const void *r2) {
    return (*(Range**)r1)->lo - (*(Range**)r2)->lo;
}

int main(int argc, char **argv) {
    START_TIMER();

    List sensors = list_empty();
    List part1_exclude_ranges = list_empty();

    char *line = NULL;
    while (get_line(&line, stdin) != -1) {
        Sensor *sensor = malloc(sizeof(Sensor));
        sscanf(
            line,
            "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
            &sensor->x,
            &sensor->y,
            &sensor->beacon.x,
            &sensor->beacon.y
        );
        list_append(&sensors, sensor);

        // part 1
        Range *range = range_excluded(sensor, TARGET_Y);
        if (range != NULL) {
            if (
                sensor->beacon.y == TARGET_Y &&
                range->lo <= sensor->beacon.x && sensor->beacon.x <= range->hi
            ) {
                // exclude beacon itself from range
                Range *range_before = malloc(sizeof(Range));
                range_before->lo = range->lo;
                range_before->hi = sensor->beacon.x - 1;
                list_append(&part1_exclude_ranges, range_before);

                // modify range to start after beacon
                range->lo = sensor->beacon.x + 1;
            }
            list_append(&part1_exclude_ranges, range);
        }
    }

    // part 1
    qsort(part1_exclude_ranges.contents, part1_exclude_ranges.length, sizeof(Range*), cmp_range);
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
        free(range);
    }
    list_free(part1_exclude_ranges);

    // part 2
    uint64_t distress_beacon_x = -1, distress_beacon_y = -1;
    for (int target_y = 0; target_y <= MAX_COORD; target_y++) {
        List exclude_ranges = list_empty();
        for (int i = 0; i < sensors.length; i++) {
            Sensor *sensor = list_get(sensors, i);
            Range *range = range_excluded(sensor, target_y);
            if (range != NULL) {
                list_append(&exclude_ranges, range);
            }
        }
        qsort(exclude_ranges.contents, exclude_ranges.length, sizeof(Range*), cmp_range);
        int prev = -1;
        for (int i = 0; i < exclude_ranges.length && prev < MAX_COORD; i++) {
            Range *range = list_get(exclude_ranges, i);
            if (range->lo > prev + 1) {
                distress_beacon_x = prev + 1;
                distress_beacon_y = target_y;
                goto found_distress_beacon;
            }
            prev = max(prev, range->hi);
            free(range);
        }
        list_free(exclude_ranges);
    }
found_distress_beacon:;

    printf("Part 1 (y = %d): %d\n", TARGET_Y, part1_count);
    printf("Part 2: %llu\n", distress_beacon_x * 4000000 + distress_beacon_y);

    END_TIMER();
    return 0;
}
