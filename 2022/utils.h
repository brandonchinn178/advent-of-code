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

    /***** Numbers *****/

    #define signum(x) (x > 0 ? 1 : x < 0 ? -1 : 0)
    #define min(x, y) (x < y ? x : y)
    #define max(x, y) (x > y ? x : y)

    /***** Strings *****/

    extern bool is_prefix(const char* s, const char* pre);

    /***** Lists *****/

    typedef struct {
        void** contents;
        size_t length;
        size_t _contents_size;
    } List;
    extern List list_empty();
    extern List list_init(size_t size);
    extern void list_append(List* list, void* item);
    extern void list_set(List *list, size_t index, void *item);
    extern void* list_get(List list, size_t index);

    /**
     * Free the allocated list (but _not_ the list items themselves!)
     */
    extern void list_free(List list);

    /***** Sorting *****/

    int cmp_int_asc(const void *a, const void *b);
    int cmp_int_desc(const void *a, const void *b);
#endif
