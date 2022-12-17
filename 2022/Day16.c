#include "./utils.h"

/*
Overview:

Keep track of max-heap of states containing:
    * how much time left
    * what valves have been opened
    * current psi_released

where the value of a state is:

    psi_released + flow_rate1 * time + flow_rate2 * (time - 1) + ...

where `flow_rateN` is the Nth-highest-flow-rate of all unopened values,
which is an upper bound on the psi_released in that state.
*/

/**
 * Guaranteed to be a 2-character string with a null terminator.
 */
typedef char* ValveName;

typedef struct {
    /**
     * Index of valve in Valves.list. Initialized to -1.
     */
    int id;

    ValveName name;
    int flow_rate;

    ValveName *neighbors;
    size_t num_neighbors;
} Valve;

static Valve* parse_valve(char *line, size_t line_len) {
    Valve *valve = malloc(sizeof(Valve));
    valve->id = -1;
    valve->name = calloc(sizeof(char), 3);
    sscanf(
        line,
        "Valve %c%c has flow rate=%d;",
        &valve->name[0],
        &valve->name[1],
        &valve->flow_rate
    );

    int i = 0;
    while (line[i] != ';') {
        i++;
    }
    i += 2;
    if (is_prefix(line + i, "tunnels ")) {
        i += strlen("tunnels lead to valves ");
    } else {
        i += strlen("tunnel leads to valve ");
    }

    List neighbors = list_empty();
    while (i < line_len) {
        ValveName neighbor = calloc(sizeof(char), 3);
        neighbor[0] = line[i];
        neighbor[1] = line[i + 1];
        list_append(&neighbors, neighbor);
        i += 4;
    }

    valve->neighbors = (ValveName*) neighbors.contents;
    valve->num_neighbors = neighbors.length;

    return valve;
}

static int cmp_valve_by_name(const void *a, const void *b) {
    return strcmp((*(Valve**)a)->name, (*(Valve**)b)->name);
}

static int cmp_valve_by_flow_rate_desc(const void *a, const void *b) {
    return -1 * ((*(Valve**) a)->flow_rate - (*(Valve**) b)->flow_rate);
}

/***** Valves *****/

typedef struct {
    size_t length;

    /**
     * Valves sorted by name
     */
    Valve* *list;

    /**
     * Indexes pointing into `list`, sorted by flow rate
     */
    int *ids_by_flow_rate;

    /**
     * 2D matrix of size (num_valves x num_valves) where
     * distances[valve_i][valve_j] contains the distance
     * from valve at list[i] to valve at list[j]
     */
    int **distances;
} Valves;

static int* _init_valve_distances(Valves *valves, Valve *start_valve);
static Valve* get_valve(Valves *valves, ValveName name);

static Valves init_valves(List *valves_list) {
    Valves valves;

    size_t num_valves = valves_list->length;
    valves.length = num_valves;
    valves.list = (Valve**) valves_list->contents;

    // first sort by flow rate, and copy names
    qsort(valves.list, num_valves, sizeof(Valve*), cmp_valve_by_flow_rate_desc);
    ValveName *names_by_flow_rate = malloc(num_valves * sizeof(ValveName));
    for (int i = 0; i < num_valves; i++) {
        Valve *valve = valves.list[i];
        names_by_flow_rate[i] = valve->name;
    }

    // then sort by name + save ID
    qsort(valves.list, num_valves, sizeof(Valve*), cmp_valve_by_name);
    for (int i = 0; i < num_valves; i++) {
        valves.list[i]->id = i;
    }

    // convert flow_rate-sorted names into flow_rate-sorted ids
    valves.ids_by_flow_rate = malloc(num_valves * sizeof(int));
    for (int i = 0; i < num_valves; i++) {
        valves.ids_by_flow_rate[i] = get_valve(&valves, names_by_flow_rate[i])->id;
    }

    // calculate distances
    valves.distances = malloc(num_valves * sizeof(int*));
    for (int i = 0; i < num_valves; i++) {
        valves.distances[i] = _init_valve_distances(&valves, valves.list[i]);
    }

    return valves;
}

static int* _init_valve_distances(Valves *valves, Valve *start_valve) {
    // distances[i] is the distance from start_valve to valve i
    int *distances = malloc(valves->length * sizeof(int));
    for (int i = 0; i < valves->length; i++) {
        distances[i] = -1;
    }

    List buf = list_empty();
    List buf_next = list_empty();
    list_append(&buf, start_valve);

    int dist = 0;

    while (buf.length > 0) {
        for (int i = 0; i < buf.length; i++) {
            Valve *valve = list_get(buf, i);

            if (distances[valve->id] != -1) {
                continue;
            }
            distances[valve->id] = dist;

            for (int j = 0; j < valve->num_neighbors; j++) {
                ValveName neighbor_name = valve->neighbors[j];
                int neighbor_id = get_valve(valves, neighbor_name)->id;
                Valve *neighbor = valves->list[neighbor_id];
                if (distances[neighbor_id] == -1) {
                    list_append(&buf_next, neighbor);
                }
            }
        }

        // swap buf + buf_next, reset buf_next
        List tmp = buf;
        buf = buf_next;
        buf_next = tmp;
        buf_next.length = 0;

        dist++;
    }

    return distances;
}

/**
 * Get the valve with the given name
 */
static Valve* get_valve(Valves *valves, ValveName name) {
    int l = 0, r = valves->length;
    while (l < r) {
        int i = (l + r) / 2;
        Valve *valve = valves->list[i];
        int cmp_result = strcmp(name, valve->name);
        if (cmp_result == 0) {
            return valve;
        } else if (cmp_result < 0) {
            r = i;
        } else {
            l = i + 1;
        }
    }
    ABORT("Could not find ID for valve %c%c", name[0], name[1]);
}

/***** ValvesHeap *****/

typedef struct {
    Valve *curr_valve;
    int time_remaining;
    int psi_released;

    /**
     * Array of `num_valves` bools, where `true` = valve has been opened
     */
    bool *valves_opened;

    /**
     * Value of the state, cached for quick reference
     */
    int value;
} ValveState;

typedef struct {
    ValveState* *nodes;
    size_t capacity;
} ValvesHeap;

static void insert_valves_heap(ValvesHeap *heap, ValveState *state);
static int get_state_value(Valves *valves, ValveState *state);

static ValvesHeap init_valves_heap(Valves *valves) {
    size_t initial_capacity = 256;
    ValveState* *nodes = calloc(sizeof(ValveState*), initial_capacity);
    ValvesHeap heap = (ValvesHeap) {
        .nodes = nodes,
        .capacity = initial_capacity,
    };

    bool *valves_opened = calloc(sizeof(bool), valves->length);

    // open all valves with flow_rate = 0, to avoid paths that open a useless valve
    for (int i = 0; i < valves->length; i++) {
        if (valves->list[i]->flow_rate == 0) {
            valves_opened[i] = true;
        }
    }

    ValveState *state = malloc(sizeof(ValveState));
    *state = (ValveState) {
        .curr_valve = get_valve(valves, "AA"),
        .time_remaining = 30,
        .psi_released = 0,
        .valves_opened = valves_opened,
    };
    state->value = get_state_value(valves, state);
    insert_valves_heap(&heap, state);

    return heap;
}

static bool* copy_valves_opened(bool *valves_opened, size_t num_valves) {
    bool *copy = malloc(num_valves * sizeof(bool));
    memmove(copy, valves_opened, num_valves * sizeof(bool));
    return copy;
}

static int get_state_value(Valves *valves, ValveState *state) {
    int potential_psi_released = 0;
    int time_remaining = state->time_remaining;
    for (int i = 0; i < valves->length && time_remaining > 0; i++) {
        int valve_id = valves->ids_by_flow_rate[i];
        if (!state->valves_opened[valve_id]) {
            Valve *valve = valves->list[valve_id];
            potential_psi_released += valve->flow_rate * time_remaining;
            time_remaining--;
        }
    }
    return state->psi_released + potential_psi_released;
}

static bool heap_is_empty(ValvesHeap *heap) {
    return heap->nodes[0] == NULL;
}

static void _heap_sift_down(ValvesHeap *heap, int idx) {
    // find child with higher value
    ValveState *max_child = NULL;
    int max_child_idx = -1;
    for (int i = 1; i <= 2; i++) {
        int child_idx = idx * 2 + i;
        if (child_idx >= heap->capacity) {
            continue;
        }

        ValveState *child = heap->nodes[child_idx];
        if (child == NULL) {
            continue;
        }
        if (max_child == NULL || child->value > max_child->value) {
            max_child_idx = child_idx;
            max_child = child;
        }
    }

    // if no children, exit
    if (max_child == NULL) {
        return;
    }

    ValveState *state = heap->nodes[idx];
    if (max_child->value > state->value) {
        heap->nodes[idx] = max_child;
        heap->nodes[max_child_idx] = state;
        _heap_sift_down(heap, max_child_idx);
    }
}

static void _heap_sift_up(ValvesHeap *heap, int idx) {
    ValveState *state = heap->nodes[idx];

    int parent_idx = (idx - 1) / 2;
    ValveState *parent = heap->nodes[parent_idx];

    if (state->value > parent->value) {
        heap->nodes[parent_idx] = state;
        heap->nodes[idx] = parent;
        _heap_sift_up(heap, parent_idx);
    }
}

static void insert_valves_heap(ValvesHeap *heap, ValveState *state) {
    // find first index with empty element
    int i = 0;
    while (i < heap->capacity) {
        if (heap->nodes[i] == NULL) break;
        i++;
    }

    // if we got all the way to the end, resize the heap
    if (i == heap->capacity) {
        size_t old_capacity = heap->capacity;
        size_t new_capacity = old_capacity * 2;
        heap->nodes = realloc(heap->nodes, new_capacity * sizeof(ValveState*));
        memset(heap->nodes + old_capacity, 0, old_capacity * sizeof(ValveState*));
        heap->capacity = new_capacity;
    }

    heap->nodes[i] = state;
    _heap_sift_up(heap, i);
}

static ValveState* pop_valves_heap(ValvesHeap *heap) {
    ValveState *root = heap->nodes[0];
    heap->nodes[0] = NULL;

    // bring last node (if one exists) to top
    for (int i = heap->capacity - 1; i > 0; i--) {
        if (heap->nodes[i] != NULL) {
            heap->nodes[0] = heap->nodes[i];
            heap->nodes[i] = NULL;
            _heap_sift_down(heap, 0);
            break;
        }
    }

    return root;
}

static void print_state(ValveState *state) {
    printf(
        "(%p,%s,min=%d,psi=%d,val=%d)",
        state,
        state->curr_valve->name,
        state->time_remaining,
        state->psi_released,
        state->value
    );
}

static void print_heap(ValvesHeap *heap) {
    printf("==================== Heap ====================\n");
    int level_end_idx = 0;
    int level_size = 1;
    for (int i = 0; i < heap->capacity; i++) {
        ValveState *state = heap->nodes[i];
        if (state == NULL) {
            printf("ø ");
        } else {
            print_state(state);
            printf(" ");
        }
        if (i == level_end_idx || i == heap->capacity - 1) {
            printf("\n");
            level_size *= 2;
            level_end_idx += level_size;
        }
    }
}

/***** Entrypoint *****/

static int get_max_psi_released(Valves *valves) {
    ValvesHeap heap = init_valves_heap(valves);

    while (!heap_is_empty(&heap)) {
        ValveState *state = pop_valves_heap(&heap);
        // print_state(state); printf("\n");
        // printf("<<<<<<<<<<<<<<<<<<<< Popped >>>>>>>>>>>>>>>>>>>>\n");
        // print_heap(&heap);
        if (state->psi_released == state->value) {
            return state->psi_released;
        }
        int curr_id = state->curr_valve->id;

        // put next states on heap
        for (int neighbor_id = 0; neighbor_id < valves->length; neighbor_id++) {
            if (state->valves_opened[neighbor_id]) {
                continue;
            }
            Valve *neighbor = valves->list[neighbor_id];

            bool *valves_opened = copy_valves_opened(state->valves_opened, valves->length);
            valves_opened[neighbor_id] = true;

            // extra -1 for opening valve
            int distance = valves->distances[curr_id][neighbor_id];
            int time_remaining = state->time_remaining - distance - 1;

            ValveState *next_state = malloc(sizeof(ValveState));
            next_state->curr_valve = neighbor;
            next_state->valves_opened = valves_opened;

            if (time_remaining <= 0) {
                next_state->time_remaining = 0;
                next_state->psi_released = state->psi_released;
                next_state->value = state->psi_released;
            } else {
                next_state->time_remaining = time_remaining;
                next_state->psi_released = state->psi_released + time_remaining * neighbor->flow_rate;
                next_state->value = get_state_value(valves, next_state);
            }

            insert_valves_heap(&heap, next_state);
            // printf("<<<<<<<<<<<<<<<<<<<< Inserted >>>>>>>>>>>>>>>>>>>>\n");
            // print_heap(&heap);
        }

        free(state->valves_opened);
        free(state);
    }

    ABORT("Exhausted all possibilities");
}

int main(int argc, char **argv) {
    START_TIMER();

    List valves_list = list_empty();

    char *line = NULL;
    size_t line_len;
    while ((line_len = get_line(&line, stdin)) != -1) {
        Valve *valve = parse_valve(line, line_len);
        list_append(&valves_list, valve);
    }

    Valves valves = init_valves(&valves_list);

    int max_psi_released = get_max_psi_released(&valves);
    printf("Part 1: %d\n", max_psi_released);

    END_TIMER();
    return 0;
}
