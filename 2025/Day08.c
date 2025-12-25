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

#define MAX_POINTS 1000

typedef long long int BigInt;

typedef int PointId;

typedef struct {
  PointId id;
  int x;
  int y;
  int z;
} Point;

typedef int CircuitId;

typedef struct {
  CircuitId point_to_circuit[MAX_POINTS];
} CircuitMap;

void CircuitMap_init(CircuitMap *circuit_map, Point points[],
                     size_t num_points) {
  for (int i = 0; i < num_points; i++) {
    Point point = points[i];
    circuit_map->point_to_circuit[point.id] = point.id;
  }
}

bool CircuitMap_all_same(CircuitMap *circuit_map, size_t num_circuits) {
  CircuitId circuit = circuit_map->point_to_circuit[0];
  for (int i = 0; i < num_circuits; i++) {
    if (circuit_map->point_to_circuit[i] != circuit) {
      return false;
    }
  }
  return true;
}

typedef struct {
  Point p1;
  Point p2;
  BigInt dist_squared; // we only care about relative order, so no need to sqrt
} Connection;

BigInt get_dist_squared(Point p1, Point p2) {
  BigInt dx = p1.x - p2.x;
  BigInt dy = p1.y - p2.y;
  BigInt dz = p1.z - p2.z;
  return (dx * dx) + (dy * dy) + (dz * dz);
}

int _compare_connections(Connection a, Connection b) {
  if (a.dist_squared < b.dist_squared) {
    return -1;
  } else if (a.dist_squared > b.dist_squared) {
    return 1;
  }
  return 0;
}

void _swap_connections(Connection *a, Connection *b) {
  Connection tmp = *a;
  *a = *b;
  *b = tmp;
}

void _quick_sort_connections(Connection connections[], int start, int end) {
  if (start >= end) {
    return;
  }

  Connection pivot = connections[end];
  int numLT = 0; // number of elements smaller than pivot
  for (int i = start; i < end; i++) {
    if (_compare_connections(connections[i], pivot) < 0) {
      _swap_connections(&connections[i], &connections[start + numLT]);
      numLT++;
    }
  }
  // Swap pivot into position
  _swap_connections(&connections[start + numLT], &connections[end]);

  _quick_sort_connections(connections, start, start + numLT - 1);
  _quick_sort_connections(connections, start + numLT + 1, end);
}

void sort_connections(Connection connections[], size_t num_connections) {
  return _quick_sort_connections(connections, 0, num_connections - 1);
}

/***** Parsing *****/

void process_line(int *part1_stop, Point points[], size_t *num_points,
                  char *line, size_t line_len, size_t line_num) {
  if (line_num == 0) {
    int n = 0;
    for (int i = 0; i < line_len; i++) {
      char c = line[i];
      if (c < '0' || '9' < c) {
        break;
      }
      n = n * 10 + (c - '0');
    }
    *part1_stop = n;
    return;
  }

  int x = 0, y = 0, z = 0;
  int *pos = &x;
  for (int i = 0; i < line_len; i++) {
    char c = line[i];
    if (c == ',') {
      pos = pos == &x ? &y : &z;
    } else {
      *pos = *pos * 10 + (c - '0');
    }
  }

  int index = (*num_points)++;
  points[index] = (Point){
      .id = index,
      .x = x,
      .y = y,
      .z = z,
  };
}

void load_input(int *part1_stop, Point points[], size_t *num_points) {
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

    process_line(part1_stop, points, num_points, line, line_len, line_num);
  }
}

/***** Entrypoint *****/

BigInt calculate_part1(CircuitMap *circuit_map, size_t num_circuits) {
  int sizes[MAX_POINTS] = {0};
  for (int i = 0; i < num_circuits; i++) {
    CircuitId circuit = circuit_map->point_to_circuit[i];
    sizes[circuit]++;
  }
  int size1 = 0, size2 = 0, size3 = 0;
  for (int i = 0; i < num_circuits; i++) {
    int size = sizes[i];
    if (size > size1) {
      int tmp = size1;
      size1 = size;
      size = tmp;
    }
    if (size > size2) {
      int tmp = size2;
      size2 = size;
      size = tmp;
    }
    if (size > size3) {
      int tmp = size3;
      size3 = size;
      size = tmp;
    }
  }
  return size1 * size2 * size3;
}

BigInt calculate_part2(Point p1, Point p2) { return p1.x * p2.x; }

Result run() {
  int part1_stop;
  Point points[MAX_POINTS];
  size_t num_points = 0;
  load_input(&part1_stop, points, &num_points);

  CircuitMap circuit_map;
  CircuitMap_init(&circuit_map, points, num_points);
  size_t num_circuits = num_points;

  size_t num_connections = num_points * (num_points - 1) / 2;
  Connection *connections = calloc(num_connections, sizeof(*connections));
  if (connections == NULL) {
    return ERR("Could not allocate memory for connections");
  }

  int conn_idx = 0;
  for (int i = 0; i < num_points; i++) {
    for (int j = i + 1; j < num_points; j++) {
      Point p1 = points[i];
      Point p2 = points[j];
      connections[conn_idx++] = (Connection){
          .p1 = p1,
          .p2 = p2,
          .dist_squared = get_dist_squared(p1, p2),
      };
    }
  }
  sort_connections(connections, num_connections);

  BigInt part1_result = -1, part2_result = -1;
  for (int i = 0; i < num_connections; i++) {
    if (i == part1_stop) {
      part1_result = calculate_part1(&circuit_map, num_circuits);
    }

    Connection conn = connections[i];
    CircuitId c1 = circuit_map.point_to_circuit[conn.p1.id];
    CircuitId c2 = circuit_map.point_to_circuit[conn.p2.id];
    if (c1 != c2) {
      for (int i = 0; i < num_circuits; i++) {
        CircuitId *circuit = &circuit_map.point_to_circuit[i];
        if (*circuit == c1) {
          *circuit = c2;
        }
      }
    }

    if (CircuitMap_all_same(&circuit_map, num_circuits)) {
      part2_result = calculate_part2(conn.p1, conn.p2);
      break;
    }
  }

  printf("Part 1: %lld\n", part1_result);
  printf("Part 2: %lld\n", part2_result);

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
