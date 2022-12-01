#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./utils.h"

char* freadall(const char* filename) {
    FILE* fp = fopen(filename, "r");
    if (!fp) {
        return NULL;
    }

    fseek(fp, 0, SEEK_END);
    int fsize = ftell(fp);
    rewind(fp);

    char* fcontent = (char*) malloc(fsize * sizeof(char));
    fread(fcontent, 1, fsize, fp);

    fclose(fp);

    return fcontent;
}

/***** Lines *****/

void free_lines(Lines lines) {
    free(lines.lines);
}

Lines split_lines(char* s) {
    int s_len = strlen(s);

    int num_lines = 1;
    for (int i = 0; i < s_len; i++) {
        if (s[i] == '\n') {
            num_lines++;
        }
    }

    char** lines = malloc(num_lines * sizeof(char*));

    char* buf_start = s;
    int buf_len = 0;
    int curr_line = 0;
    for (int i = 0; i <= s_len; i++) {
        if (i == s_len || s[i] == '\n') {
            lines[curr_line] = buf_start;
            curr_line++;

            // skip '\n' character
            buf_start = buf_start + buf_len + 1;
            buf_len = 0;
        } else {
            buf_len++;
        }
    }

    return (Lines) { .length = num_lines, .lines = lines };
}

char* get_line(Lines lines, int i) {
    char* line = lines.lines[i];

    size_t line_len;
    if (i == lines.length - 1) {
        line_len = strlen(line);
    } else {
        char* next_line = lines.lines[i + 1];
        line_len = next_line - line - 1;
    }

    char* res = malloc(line_len * sizeof(char) + 1);
    memcpy(res, line, line_len * sizeof(char));
    res[line_len] = '\0';
    return res;
}

/***** Sorting *****/

static void merge_sort_inplace(int* arr, int l, int r) {
    if (l >= r) {
        return;
    }

    int total = r - l;
    int m = l + total / 2;
    merge_sort_inplace(arr, l, m);
    merge_sort_inplace(arr, m + 1, r);

    // copy data to temp arrays
    int num_l = m - l + 1, num_r = r - m;
    int arr_l[num_l], arr_r[num_r];
    for (int i = 0; i < num_l; i++) {
        arr_l[i] = arr[l + i];
    }
    for (int j = 0; j < num_r; j++) {
        arr_r[j] = arr[l + num_l + j];
    }

    // merge sorted temp arrays
    int i = 0;
    int j = 0;
    while (i + j <= total) {
        int k = l + i + j;
        if (i >= num_l) {
            arr[k] = arr_r[j];
            j++;
        } else if (j >= num_r) {
            arr[k] = arr_l[i];
            i++;
        } else if (arr_l[i] <= arr_r[j]) {
            arr[k] = arr_l[i];
            i++;
        } else {
            arr[k] = arr_r[j];
            j++;
        }
    }
}

void sort_list_inplace(int* arr, int len) {
    merge_sort_inplace(arr, 0, len - 1);
}
