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

    /***** Sorting *****/
    typedef enum { ASC, DESC } Ordering;
    extern void sort_list_inplace(int* arr, int len, Ordering ord);
#endif
