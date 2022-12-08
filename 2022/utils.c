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

void* list_get(List list, size_t index) {
    return list.contents[index];
}

/***** Sorting *****/

static void merge_sort_inplace(int* arr, int l, int r, Ordering ord) {
    if (l >= r) {
        return;
    }

    int total = r - l;
    int m = l + total / 2;
    merge_sort_inplace(arr, l, m, ord);
    merge_sort_inplace(arr, m + 1, r, ord);

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
        int use_l;
        if (i >= num_l) {
            use_l = false;
        } else if (j >= num_r) {
            use_l = true;
        } else {
            switch (ord) {
                case ASC: use_l = arr_l[i] <= arr_r[j]; break;
                case DESC: use_l = arr_l[i] >= arr_r[j]; break;
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

void sort_list_inplace(int* arr, int len, Ordering ord) {
    merge_sort_inplace(arr, 0, len - 1, ord);
}
