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
    char* line = NULL;

    int part1_score = 0;
    int part2_score = 0;

    while (get_line(&line, stdin) != -1) {
        char opp_sym, other_sym;
        sscanf(line, "%c %c", &opp_sym, &other_sym);
        RPS opp = parse_rps(opp_sym);

        // part 1
        RPS me1 = parse_rps(other_sym);
        Outcome outcome1 = get_outcome(me1, opp);
        part1_score += get_shape_score(me1);
        part1_score += get_outcome_score(outcome1);

        // part 2
        Outcome outcome2 = parse_outcome(other_sym);
        RPS me2 = get_shape_for_outcome(opp, outcome2);
        part2_score += get_shape_score(me2);
        part2_score += get_outcome_score(outcome2);
    }

    printf("Part 1 score: %d\n", part1_score);
    printf("Part 2 score: %d\n", part2_score);

    return 0;
}
