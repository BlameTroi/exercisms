// exercism armstrong numbers
// t.brumley june 2022

#include <stdio.h> 
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <string.h>

#include "armstrong_numbers.h"

// is a given integer an armstrong number?
//
// an armstrong number is one where the sum of its digits each raised
// to the power of the number of digits equals the number.
//
// 9 -> 9^1 = 9 ** is **
// 10 -> 1^2 + 0^2 = 1 ** not **
// 4 -> 4^1 = 4 ** is **

bool is_armstrong_number(int candidate)
{
    // quick guards for some easy cases
    if (candidate >= 0 && candidate < 10)
        return true;
    if (candidate > 9 && candidate < 100)
        return false;

    // the exercise instructions are to use standard library support. i'd
    // prefer to avoid the bloat, but i'll follow the instructions.

    // convert to string for easy access to digits
    // SB_LENGTH includes trailing null byte
#define SB_LENGTH 32
    char sb[SB_LENGTH];
    snprintf(sb, SB_LENGTH, "%d", candidate);

    // sum each digit raised to the number of digits
    int sum = 0;
    double pwr = strlen(sb);
    for(char* p = sb; *p != '\0'; p++) {
        int d = *p - '0';
        sum += (int)(pow((double)d, pwr));
    }

    // if sum and original value match, the eagle has landed
    return sum == candidate;

}
