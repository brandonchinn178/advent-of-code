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

int cmp_int_asc(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int cmp_int_desc(const void *a, const void *b) {
    return -1 * cmp_int_asc(a, b);
}
