#ifndef _UTILS_H_
    #define _UTILS_H_

    // include common libraries
    #include <stdbool.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <time.h>

    /***** Errors *****/

    #define ABORT(msg, ...) \
        fprintf(stderr, "%s:%d: " msg "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
        exit(1);

    /***** Timer *****/

    #define START_TIMER() \
        clock_t start_time = clock();
    #define END_TIMER() \
        double elapsed_time = (double) (clock() - start_time) / CLOCKS_PER_SEC; \
        printf("Done in %f seconds\n", elapsed_time);

    /***** Files *****/

    /**
     * Same as getline(), except throwing away the buf_size argument
     * and removing trailing newline.
     */
    extern size_t get_line(char** buf, FILE* stream);

    /***** Strings *****/

    extern bool is_prefix(const char* s, const char* pre);

    /***** Sorting *****/

    typedef enum { ASC, DESC } Ordering;
    extern void sort_list_inplace(int* arr, int len, Ordering ord);
#endif
