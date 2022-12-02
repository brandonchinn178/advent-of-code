#ifndef _UTILS_H_
    #define _UTILS_H_

    #include <stdio.h>
    #include <stdlib.h>

    /***** Errors *****/
    #define ABORT(msg, ...) \
        fprintf(stderr, "%s:%d: " msg "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
        exit(1);

    /***** Files *****/
    #ifdef SAMPLE
        #define INPUT_SUFFIX "-sample"
    #else
        #define INPUT_SUFFIX ""
    #endif
    #define READ_INPUT(day) freadall("data/" day INPUT_SUFFIX ".txt")
    extern char* freadall(const char* filename);

    /***** Lines *****/
    typedef struct {
        int length;
        /* _Not_ null-terminated */
        char** lines;
    } Lines;
    extern void free_lines(Lines lines);
    extern Lines split_lines(char* s);
    extern char* get_line(Lines lines, int i);

    /***** Sorting *****/
    extern void sort_list_inplace(int* arr, int len);
#endif
