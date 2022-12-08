#include "./utils.h"

typedef enum { ENTRY_FILE, ENTRY_DIR } EntryType;

typedef struct Directory {
    char *name;
    EntryType type;

    // parent directory
    struct Directory *parent;

    // list of `Entry`s
    List entries;

    // initially 0, populated in get_dir_size
    size_t _size;
} Directory;

typedef struct {
    char *name;
    EntryType type;

    size_t size;
} File;

typedef union {
    struct {
        char *name;
        EntryType type;
    } common;

    Directory dir;
    File file;
} Entry;

/***** Creation *****/

Directory init_dir(char *name, Directory *parent) {
    return (Directory) {
        .name = name,
        .type = ENTRY_DIR,
        .parent = parent,
        .entries = list_empty(),
        ._size = 0,
    };
}

void mkdir(Directory *cwd, char *name) {
    Directory *dir = malloc(sizeof(Directory));
    *dir = init_dir(name, cwd);
    list_append(&cwd->entries, dir);
}

void touch(Directory *cwd, char *name, size_t size) {
    File *file = malloc(sizeof(File));
    *file = (File) {
        .name = name,
        .type = ENTRY_FILE,
        .size = size,
    };
    list_append(&cwd->entries, file);
}

/***** Queries *****/

Entry* get_entry(Directory dir, char *name) {
    for (int i = 0; i < dir.entries.length; i++) {
        Entry *entry = list_get(dir.entries, i);
        if (strcmp(name, entry->common.name) == 0) {
            return entry;
        }
    }
    return NULL;
}

size_t get_size(Entry *entry);

size_t get_dir_size(Directory *dir) {
    if (dir->_size == 0) {
        for (int i = 0; i < dir->entries.length; i++) {
            Entry *entry = list_get(dir->entries, i);
            dir->_size += get_size(entry);
        }
    }
    return dir->_size;
}

size_t get_size(Entry *entry) {
    switch (entry->common.type) {
        case ENTRY_FILE:
            return entry->file.size;
        case ENTRY_DIR:
            return get_dir_size(&entry->dir);
    }
}

/***** Debugging *****/

void print_indent(size_t level) {
    for (int i = 0; i < level; i++) {
        printf("  ");
    }
}

void print_file_indent(File file, size_t level) {
    print_indent(level);
    printf("- %s (file, size=%ld)\n", file.name, file.size);
}

void print_dir_indent(Directory dir, size_t level) {
    print_indent(level);
    printf("- %s (dir, size=%ld)\n", dir.name, dir._size);

    size_t next_level = level + 1;
    for (int i = 0; i < dir.entries.length; i++) {
        Entry *entry = list_get(dir.entries, i);
        switch (entry->common.type) {
            case ENTRY_FILE:
                print_file_indent(entry->file, next_level);
                break;
            case ENTRY_DIR:
                print_dir_indent(entry->dir, next_level);
                break;
        }
    }
}

void print_dir(Directory dir) {
    print_dir_indent(dir, 0);
}

/***** Entrypoint *****/

size_t TOTAL_SPACE = 70000000;

size_t get_part1(Directory dir) {
    size_t size = 0;
    size_t dir_size = get_dir_size(&dir);
    if (dir_size <= 100000) {
        size += dir_size;
    }

    for (int i = 0; i < dir.entries.length; i++) {
        Entry *entry = list_get(dir.entries, i);
        if (entry->common.type == ENTRY_DIR) {
            size += get_part1(entry->dir);
        }
    }

    return size;
}

size_t get_part2(Directory dir, size_t space_to_delete) {
    size_t smallest_freeable_size = TOTAL_SPACE;

    for (int i = 0; i < dir.entries.length; i++) {
        Entry *entry = list_get(dir.entries, i);
        if (entry->common.type == ENTRY_DIR) {
            size_t smallest_freeable_size_entry = get_part2(entry->dir, space_to_delete);
            if (smallest_freeable_size_entry < smallest_freeable_size) {
                smallest_freeable_size = smallest_freeable_size_entry;
            }
        }
    }

    size_t dir_size = get_dir_size(&dir);
    if (dir_size >= space_to_delete && dir_size < smallest_freeable_size) {
        smallest_freeable_size = dir_size;
    }

    return smallest_freeable_size;
}

int main(int argc, char **argv) {
    START_TIMER();

    char *line = NULL;
    size_t line_len;

    Directory root = init_dir("/", NULL);
    Directory *cwd = &root;

    while ((line_len = get_line(&line, stdin)) != -1) {
        if (is_prefix(line, "$ cd")) {
            char* arg = line + 5;
            if (strcmp(arg, "/") == 0) {
                cwd = &root;
            } else if (strcmp(arg, "..") == 0) {
                cwd = cwd->parent;
            } else {
                Entry *entry = get_entry(*cwd, arg);
                if (entry == NULL || entry->common.type != ENTRY_DIR) {
                    ABORT("Could not find directory named %s", arg);
                }
                cwd = &entry->dir;
            }
        } else if (is_prefix(line, "$ ls")) {
            // ignore
        } else {
            if (is_prefix(line, "dir")) {
                char *name = malloc((line_len - 4 + 1) * sizeof(char));
                strcpy(name, line + 4);
                mkdir(cwd, name);
            } else {
                char *name = malloc((line_len + 1) * sizeof(char));
                size_t size;
                sscanf(line, "%ld %s", &size, name);
                touch(cwd, name, size);
            }
        }
    }

    // printf("========== After parsing ==========\n");
    // print_dir(root);

    // prepopulate directory sizes
    size_t total_usage = get_dir_size(&root);

    // part 1
    size_t part1_total = get_part1(root);
    printf("Part 1: %ld\n", part1_total);

    // part 2
    size_t space_required = 30000000;
    size_t space_to_delete = total_usage + space_required - TOTAL_SPACE;
    size_t part2_size = get_part2(root, space_to_delete);
    printf("Part 2: %ld\n", part2_size);

    END_TIMER();
    return 0;
}
