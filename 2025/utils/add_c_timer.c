#include <stdio.h>
#include <time.h>

extern int __real_main(int argc, char **argv);

#undef main // suppress the -Dmain=real_main macro
int main(int argc, char **argv) {
  struct timespec start, end;

  clock_gettime(CLOCK_MONOTONIC, &start);
  int result = __real_main(argc, argv);
  clock_gettime(CLOCK_MONOTONIC, &end);

  double elapsed = (end.tv_sec - start.tv_sec) +
                   (end.tv_nsec - start.tv_nsec) / 1000000.0;
  printf("[duration.c] Execution took: %.3f ms\n", elapsed);

  return result;
}
