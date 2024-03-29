#include "./utils.h"

/***** Streams *****/

typedef enum {
  STREAM_LAZY,
  STREAM_SINGLE_INT,
} StreamType;

// represents a stream generated from the given list, where
// contents[index] represents the current state of the stream.
typedef struct {
    StreamType type;
    char *contents;
    int index;
} StreamLazy;

// represents a stream with one element: value.
typedef struct {
    StreamType type;
    int value;
    bool consumed;
} StreamSingle;

typedef union {
    struct { StreamType type; } common;
    StreamLazy lazy;
    StreamSingle single;
} Stream;

static Stream* stream_init(char *contents0) {
    Stream *stream = malloc(sizeof(Stream));
    char *contents = malloc((strlen(contents0) + 1) * sizeof(char));
    strcpy(contents, contents0);
    *stream = (Stream) {
        .lazy = {
            .type = STREAM_LAZY,
            .contents = contents,
            .index = 0,
        },
    };
    return stream;
}

static Stream stream_single(int value) {
    return (Stream) {
        .single = {
            .type = STREAM_SINGLE_INT,
            .value = value,
            .consumed = false,
        },
    };
}

static void stream_lazy_advance(StreamLazy *stream) {
    stream->index++;
}

static char stream_lazy_peek(StreamLazy *stream) {
    return stream->contents[stream->index];
}

static char stream_lazy_get(StreamLazy *stream) {
    char c = stream_lazy_peek(stream);
    stream_lazy_advance(stream);
    return c;
}

static void stream_start(Stream *stream) {
    switch (stream->common.type) {
        case STREAM_LAZY: {
            char c = stream_lazy_get(&stream->lazy);
            if (c != '[') {
                ABORT("Expected start of list, got: %c", c);
            }
            return;
        }
        case STREAM_SINGLE_INT:
            return;
    }
}

static void stream_comma(Stream *stream) {
    switch (stream->common.type) {
        case STREAM_LAZY: {
            char c = stream_lazy_peek(&stream->lazy);
            if (c == ',') {
                stream_lazy_advance(&stream->lazy);
            }
            return;
        }
        case STREAM_SINGLE_INT:
            return;
    }
}

static void stream_reset(Stream *stream) {
    switch (stream->common.type) {
        case STREAM_LAZY: {
            stream->lazy.index = 0;
            return;
        }
        case STREAM_SINGLE_INT: {
            stream->single.consumed = false;
            return;
        }
    }
}

/***** Element *****/

typedef enum {
    ELEMENT_INT,
    ELEMENT_LIST,
    ELEMENT_NONE,
} ElementType;

typedef union {
    struct { ElementType type; } common;
    struct { ElementType type; int value; } integer;
    struct { ElementType type; Stream *stream; } list;

    // returned when list is out of elements
    struct { ElementType type; } none;
} Element;

static Element stream_next_element(Stream *stream) {
    switch (stream->common.type) {
        case STREAM_LAZY: {
            StreamLazy *s = &stream->lazy;
            char c = stream_lazy_peek(s);
            if (c == ']') {
                stream_lazy_advance(s);
                return (Element) { .none.type = ELEMENT_NONE };
            }
            if (c == '[') {
                return (Element) {
                    .list = {
                        .type = ELEMENT_LIST,
                        .stream = stream,
                    },
                };
            }
            if (c < '0' || '9' < c) {
                ABORT("Unexpected character: %c", c);
            }
            int n = 0;
            while (true) {
                char c = stream_lazy_peek(s);
                if (c < '0' || '9' < c) break;
                stream_lazy_advance(s);
                n = n * 10 + (c - '0');
            }
            return (Element) {
                .integer = {
                    .type = ELEMENT_INT,
                    .value = n,
                },
            };
        }
        case STREAM_SINGLE_INT: {
            if (stream->single.consumed) {
                return (Element) { .none.type = ELEMENT_NONE };
            }
            stream->single.consumed = true;
            return (Element) {
                .integer = {
                    .type = ELEMENT_INT,
                    .value = stream->single.value,
                },
            };
        }
    }
}

/***** Compare *****/

static int cmp_stream(Stream *stream1, Stream *stream2) {
    stream_start(stream1);
    stream_start(stream2);

    while (true) {
        Element elem1 = stream_next_element(stream1);
        Element elem2 = stream_next_element(stream2);
        ElementType type1 = elem1.common.type;
        ElementType type2 = elem2.common.type;

        // check end of list
        if (type1 == ELEMENT_NONE && type2 == ELEMENT_NONE) {
            return 0;
        } else if (type1 == ELEMENT_NONE) {
            return -1;
        } else if (type2 == ELEMENT_NONE) {
            return 1;
        }

        int cmp_result;
        if (type1 == ELEMENT_INT && type2 == ELEMENT_INT) {
            cmp_result = elem1.integer.value - elem2.integer.value;
        } else if (type1 == ELEMENT_INT && type2 == ELEMENT_LIST) {
            Stream tmp = stream_single(elem1.integer.value);
            cmp_result = cmp_stream(&tmp, elem2.list.stream);
        } else if (type1 == ELEMENT_LIST && type2 == ELEMENT_INT) {
            Stream tmp = stream_single(elem2.integer.value);
            cmp_result = cmp_stream(elem1.list.stream, &tmp);
        } else {
            cmp_result = cmp_stream(elem1.list.stream, elem2.list.stream);
        }
        if (cmp_result != 0) return cmp_result;

        stream_comma(stream1);
        stream_comma(stream2);
    }
}

static int cmp_full_stream(const void *stream1, const void *stream2) {
    int result = cmp_stream((Stream*) stream1, (Stream*) stream2);
    stream_reset((Stream*) stream1);
    stream_reset((Stream*) stream2);
    return result;
}

/***** Entrypoint *****/

int main(int argc, char **argv) {
    START_TIMER();

    int part1_total = 0;
    int curr_index = 1;

    List all_lines = list_empty();

    char *line1 = NULL, *line2 = NULL;
    while (get_line(&line1, stdin) != -1) {
        Stream *stream1 = stream_init(line1);

        get_line(&line2, stdin);
        Stream *stream2 = stream_init(line2);

        // skip empty line
        get_line(&line1, stdin);

        // part 1
        if (cmp_full_stream(stream1, stream2) < 0) {
            part1_total += curr_index;
        }

        // part 2
        list_append(&all_lines, stream1);
        list_append(&all_lines, stream2);

        curr_index++;
    }

    // part 2

    Stream *divider1 = stream_init("[[2]]");
    Stream *divider2 = stream_init("[[6]]");
    list_append(&all_lines, divider1);
    list_append(&all_lines, divider2);

    qsort(all_lines.contents, all_lines.length, sizeof(Stream*), cmp_full_stream);

    int part2_total = 1;
    for (int i = 0; i < all_lines.length; i++) {
        Stream *s = list_get(all_lines, i);
        if (s == divider1 || s == divider2) {
            part2_total *= i + 1;
        }
    }

    // output

    printf("Part 1: %d\n", part1_total);
    printf("Part 2: %d\n", part2_total);

    END_TIMER();
    return 0;
}
