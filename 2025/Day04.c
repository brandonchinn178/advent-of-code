#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_DIM 140

typedef char* Result;
#define ERR(msg) msg
#define OK NULL
#define CHECK(e) \
    do { \
        Result r = e; \
        if (r != OK) { return r; } \
    } while (0)

typedef unsigned char Value;
typedef struct {
    Value x;
    Value y;
} Coordinate;

/***** Set_Coordinate *****/

typedef struct Node_Coordinate {
    Coordinate _value;
    struct Node_Coordinate *_next;
} Node_Coordinate;

typedef struct {
    Node_Coordinate **_buckets;
    size_t _num_buckets;
    size_t size;
} Set_Coordinate;

Result Set_Coordinate_new(Set_Coordinate **coords_ptr) {
    size_t num_buckets = 12641; // first prime number after 12622 (number of @ in data)

    Set_Coordinate *coords = malloc(sizeof(*coords));
    if (coords == NULL) {
        return ERR("Could not allocate coordinate set");
    }

    coords->_buckets = calloc(num_buckets, sizeof(*(coords->_buckets)));
    if (coords->_buckets == NULL) {
        free(coords);
        return ERR("Could not allocate buckets");
    }

    coords->_num_buckets = num_buckets;
    coords->size = 0;

    *coords_ptr = coords;
    return OK;
}

Result _Set_Coordinate_add_to_bucket(Node_Coordinate **bucket_ptr, Coordinate coord) {
    Node_Coordinate *node = malloc(sizeof(*node));
    if (node == NULL) {
        return ERR("Could not allocate new node");
    }

    node->_value = coord;

    node->_next = *bucket_ptr;
    *bucket_ptr = node;

    return OK;
}

Result Set_Coordinate_copy(Set_Coordinate *src, Set_Coordinate **dest_ptr) {
    Set_Coordinate *dest;
    CHECK(Set_Coordinate_new(&dest));
    dest->_num_buckets = src->_num_buckets;
    dest->size = src->size;

    for (int i = 0; i < src->_num_buckets; i++) {
        Node_Coordinate **bucket_ptr = &dest->_buckets[i];
        Node_Coordinate *curr = src->_buckets[i];
        while (curr != NULL) {
            CHECK(_Set_Coordinate_add_to_bucket(bucket_ptr, curr->_value));
            curr = curr->_next;
        }
    }

    *dest_ptr = dest;
    return OK;
}

size_t Set_Coordinate_size(Set_Coordinate *coords) {
    return coords->size;
}

size_t _Set_Coordinate_hash(Coordinate coord, size_t num_buckets) {
    size_t hash_x = coord.x % num_buckets;
    size_t hash_y = coord.y % num_buckets;
    return (hash_x ^ hash_y) % num_buckets;
}

bool Set_Coordinate_contains(Set_Coordinate *coords, Coordinate coord) {
    size_t idx = _Set_Coordinate_hash(coord, coords->_num_buckets);
    Node_Coordinate *node = coords->_buckets[idx];
    while (node != NULL) {
        if (node->_value.x == coord.x && node->_value.y == coord.y) {
            return true;
        }
        node = node->_next;
    }
    return false;
}

Result Set_Coordinate_add(Set_Coordinate *coords, Coordinate coord) {
    if (Set_Coordinate_contains(coords, coord)) {
        return OK;
    }

    size_t idx = _Set_Coordinate_hash(coord, coords->_num_buckets);
    Node_Coordinate **bucket_ptr = &coords->_buckets[idx];
    CHECK(_Set_Coordinate_add_to_bucket(bucket_ptr, coord));

    coords->size++;

    return OK;
}

Result Set_Coordinate_filter(
        Set_Coordinate *coords,
        bool (*should_keep)(Set_Coordinate*, Coordinate),
        int *num_removed_ptr) {
    Set_Coordinate *orig_coords;
    CHECK(Set_Coordinate_copy(coords, &orig_coords));

    for (int i = 0; i < coords->_num_buckets; i++) {
        Node_Coordinate **bucket = &coords->_buckets[i];

        Node_Coordinate *prev = NULL;
        Node_Coordinate *curr = *bucket;
        while (curr != NULL) {
            Node_Coordinate *next = curr->_next;
            if (!should_keep(orig_coords, curr->_value)) {
                if (prev == NULL) {
                    *bucket = next;
                } else {
                    prev->_next = next;
                }
                free(curr);
                curr = prev;
                coords->size--;
            }
            prev = curr;
            curr = next;
        }
    }

    *num_removed_ptr = orig_coords->size - coords->size;
    return OK;
}

/***** IntList *****/

typedef struct {
    int *data;
    size_t size;
    size_t _capacity;
} IntList;

Result IntList_new(IntList **list_ptr) {
    size_t capacity = 10; // arbitrary initial capacity

    IntList *list = malloc(sizeof(*list));
    if (list == NULL) {
        return ERR("Could not allocate list");
    }

    list->data = calloc(capacity, sizeof(*(list->data)));
    if (list->data == NULL) {
        free(list);
        return ERR("Could not allocate list data");
    }

    list->size = 0;
    list->_capacity = capacity;

    *list_ptr = list;
    return OK;
}

Result _IntList_grow(IntList *list) {
    if (list->size < list->_capacity) {
        return OK;
    }

    size_t new_capacity = list->_capacity * 2;
    int *new_data = realloc(list->data, new_capacity * sizeof(*(list->data)));
    if (new_data == NULL) {
        return ERR("Could not resize list");
    }

    list->data = new_data;
    list->_capacity = new_capacity;
    return OK;
}

Result IntList_add(IntList *list, int x) {
    CHECK(_IntList_grow(list));
    list->data[list->size++] = x;
    return OK;
}

/***** Main logic *****/

Result process_line(
        Set_Coordinate *coords,
        char *line,
        size_t line_len,
        size_t line_num) {
    Value y = line_num;
    for (int i = 0; i < line_len; i++) {
        Value x = i;
        if (line[i] == '@') {
            CHECK(Set_Coordinate_add(coords, (Coordinate){x, y}));
        }
    }
    return OK;
}

Result load_input(Set_Coordinate *coords) {
    char* line = NULL;
    size_t line_size = 0;
    for (size_t line_num = 0;; line_num++) {
        size_t line_len = getline(&line, &line_size, stdin);
        if (line_len == -1 || line_len == 0) {
            break;
        }
        if (line[line_len - 1] == '\n') {
            line_len--;
        }

        CHECK(process_line(coords, line, line_len, line_num));
    }
    return OK;
}

bool can_remove(Set_Coordinate *coords, Coordinate coord) {
    int num_neighbors = 0;
    for (int dx = -1; dx <= 1; dx++) {
        for (int dy = -1; dy <= 1; dy++) {
            if (dx == 0 && dy == 0) {
                continue;
            }

            Coordinate neighbor = {.x = coord.x + dx, .y = coord.y + dy};
            if (Set_Coordinate_contains(coords, neighbor)) {
                num_neighbors++;
            }
        }
    }

    return num_neighbors < 4;
}

bool cant_remove(Set_Coordinate *coords, Coordinate coord) {
    return !can_remove(coords, coord);
}

Result run() {
    Set_Coordinate *coords;
    CHECK(Set_Coordinate_new(&coords));

    CHECK(load_input(coords));

    IntList *rounds;
    CHECK(IntList_new(&rounds));

    int removed;
    do {
        CHECK(Set_Coordinate_filter(coords, cant_remove, &removed));
        CHECK(IntList_add(rounds, removed));
    } while (removed > 0);

    printf("Part 1: %u\n", rounds->data[0]);

    int total = 0;
    for (int i = 0; i < rounds->size; i++) {
        total += rounds->data[i];
    }
    printf("Part 2: %u\n", total);

    return 0;
}

int main(int argc, char **argv) {
    Result r = run();
    if (r != OK) {
        printf("ERROR: %s\n", r);
        return 1;
    }
    return 0;
}
