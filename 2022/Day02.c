#include <string.h>

#include "./utils.h"

typedef enum {
    ROCK,
    PAPER,
    SCISSORS,
} RPS;

static RPS parse_rps(char c) {
    switch (c) {
        case 'A': case 'X': return ROCK;
        case 'B': case 'Y': return PAPER;
        case 'C': case 'Z': return SCISSORS;
        default: ABORT("Bad RPS: %c", c);
    }
}

static int get_shape_score(RPS shape) {
    switch (shape) {
        case ROCK: return 1;
        case PAPER: return 2;
        case SCISSORS: return 3;
    }
}

typedef enum {
    WIN,
    DRAW,
    LOSE,
} Outcome;

static Outcome parse_outcome(char c) {
    switch (c) {
        case 'X': return LOSE;
        case 'Y': return DRAW;
        case 'Z': return WIN;
        default: ABORT("Bad outcome: %c", c);
    }
}

static Outcome get_outcome(RPS me, RPS opp) {
    switch ((me - opp + 3) % 3) {
        case 0: return DRAW;
        case 1: return WIN;
        case 2: return LOSE;
        default: ABORT("Could not get outcome: me=%d, opp=%d", me, opp);
    }
}

static int get_outcome_score(Outcome outcome) {
    switch (outcome) {
        case WIN: return 6;
        case DRAW: return 3;
        case LOSE: return 0;
    }
}

static RPS get_shape_for_outcome(RPS opp, Outcome outcome) {
    switch (outcome) {
        case WIN: return (opp + 1) % 3;
        case DRAW: return opp;
        case LOSE: return (opp + 2) % 3;
    }
}

int main(int argc, char **argv) {
    char* input = READ_INPUT("Day02");
    Lines lines = split_lines(input);

    // part 1
    int part1_score = 0;
    for (int i = 0; i < lines.length; i++) {
        char* line = get_line(lines, i);
        if (strlen(line) == 0) {
            continue;
        }

        char opp_sym, me_sym;
        sscanf(line, "%c %c", &opp_sym, &me_sym);

        RPS me = parse_rps(me_sym);
        RPS opp = parse_rps(opp_sym);
        Outcome outcome = get_outcome(me, opp);
        part1_score += get_shape_score(me);
        part1_score += get_outcome_score(outcome);

        free(line);
    }
    printf("Part 1 score: %d\n", part1_score);

    // part 2
    int part2_score = 0;
    for (int i = 0; i < lines.length; i++) {
        char* line = get_line(lines, i);
        if (strlen(line) == 0) {
            continue;
        }

        char opp_sym, outcome_sym;
        sscanf(line, "%c %c", &opp_sym, &outcome_sym);

        RPS opp = parse_rps(opp_sym);
        Outcome outcome = parse_outcome(outcome_sym);
        RPS me = get_shape_for_outcome(opp, outcome);
        part2_score += get_shape_score(me);
        part2_score += get_outcome_score(outcome);

        free(line);
    }
    printf("Part 2 score: %d\n", part2_score);

    free_lines(lines);
    free(input);
    return 0;
}
