#include "./utils.h"

/***** Files *****/

size_t get_line(char** line, FILE* stream) {
    size_t size = 0;
    size_t line_len = getline(line, &size, stream);
    if ((*line)[line_len - 1] == '\n') {
        (*line)[line_len - 1] = '\0';
        line_len--;
    }
    return line_len;
}

/***** Strings *****/

bool is_prefix(const char* s, const char* pre) {
    return strncmp(pre, s, strlen(pre)) == 0;
}

/***** Lists *****/

List list_empty() {
    return list_init(10);
}

List list_init(size_t size) {
    void** contents = malloc(size * sizeof(void*));
    return (List) {
        .contents = contents,
        .length = 0,
        ._contents_size = size,
    };
}

void list_append(List* list, void* item) {
    if (list->length == list->_contents_size) {
        list->_contents_size *= 2;
        list->contents = realloc(list->contents, list->_contents_size * sizeof(void*));
    }
    list->contents[list->length] = item;
    list->length++;
}

void list_set(List *list, size_t index, void *item) {
    list->contents[index] = item;
}

void* list_get(List list, size_t index) {
    return list.contents[index];
}

void list_free(List list) {
    free(list.contents);
}

/***** Sorting *****/

static void merge_sort_inplace(
    void* *arr,
    int l,
    int r,
    Direction dir,
    IsLessThan is_lt
) {
    if (l >= r) {
        return;
    }

    int total = r - l;
    int m = l + total / 2;
    merge_sort_inplace(arr, l, m, dir, is_lt);
    merge_sort_inplace(arr, m + 1, r, dir, is_lt);

    // copy data to temp arrays
    int num_l = m - l + 1, num_r = r - m;
    void* arr_l[num_l];
    void* arr_r[num_r];
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
        int use_l;
        if (i >= num_l) {
            use_l = false;
        } else if (j >= num_r) {
            use_l = true;
        } else {
            switch (dir) {
                case ASC: use_l = is_lt(arr_l[i], arr_r[j]); break;
                case DESC: use_l = !is_lt(arr_l[i], arr_r[j]); break;
            }
        }

        int k = l + i + j;
        if (use_l) {
            arr[k] = arr_l[i]; i++;
        } else {
            arr[k] = arr_r[j]; j++;
        }
    }
}

static bool is_lt_int(int *l, int *r) {
    return *l < *r;
}

void sort_int_list_inplace(int* arr, int len, Direction dir) {
    void *list[len];
    for (int i = 0; i < len; i++) {
        list[i] = &arr[i];
    }
    merge_sort_inplace(list, 0, len - 1, dir, (IsLessThan) is_lt_int);
    for (int i = 0; i < len; i++) {
        arr[i] = *((int*) list[i]);
    }
}

void sort_list_inplace(List *list, Direction dir, IsLessThan is_lt) {
    merge_sort_inplace(list->contents, 0, list->length - 1, dir, is_lt);
}
