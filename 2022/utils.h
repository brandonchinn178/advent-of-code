#ifndef _UTILS_H_
    #define _UTILS_H_

    // include common libraries
    #include <stdbool.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    /***** Errors *****/

    #define ABORT(msg, ...) \
        fprintf(stderr, "%s:%d: " msg "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
        exit(1);

    /***** Files *****/

    /**
     * Same as getline(), except throwing away the buf_size argument
     * and removing trailing newline.
     */
    extern size_t get_line(char** buf, FILE* stream);

    /***** Sorting *****/

    typedef enum { ASC, DESC } Ordering;
    extern void sort_list_inplace(int* arr, int len, Ordering ord);
#endif
