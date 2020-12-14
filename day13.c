#include <stdint.h>
#include <stdio.h>

/* My specific puzzle input, encoded as a hand-unrolled loop like Tanya's
 * kotlin version.
 *
 * Sorted by largest prime so we can go in steps of that, with indices being
 * relative to its index in the input string so we can test each quickly. */
int main(void) {
    uint64_t try = 100000000000039; // first multiple of 821 after start.

    while (1) {
        try += 821;
        if ((try + 31) % 463 != 0) continue;
        if ((try - 10) %  41 != 0) continue;
        if ((try - 6)  %  37 != 0) continue;
        if ((try + 29) %  29 != 0) continue;
        if ((try + 54) %  23 != 0) continue;
        if ((try - 19) %  19 != 0) continue;
        if ((try + 17) %  17 != 0) continue;
        if ((try + 13) %  13 != 0) continue;
        break;
    }
    printf("result: %jd\n", try);
    return 0;
}

/*
cian@waldschattenspiel:~/aoc2020$ time ./day13
result: 554865447501118

real 14m38.185s
user 14m38.023s
sys 0m0.016s
*/
