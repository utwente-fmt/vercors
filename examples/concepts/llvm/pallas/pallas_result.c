#include <stdint.h>

/*
 *  Tests to test the \result construct in Pallas C-contracts.
 */


// Big struct that will be passed as an sret argument
typedef struct s {
    int64_t a;
    int64_t b;
    int64_t c;
    int64_t d;
    int64_t e;
    int64_t f;
    int64_t g;
} BigStruct;

/*@
 declare DEF_RESULT(BigStruct);
 declare DEF_RESULT(int);
@*/

// Test that \result works with large structs that are returned in an sret-argument.

/*@
ensures RESULT(BigStruct)().a >= 0;
@*/
BigStruct fun (int a) {
    BigStruct s;
    s.a = 0;
    s.b = 1;
    return s;
}

// Test that \result works with regular values.
/*@
ensures RESULT(int)() >= 0;
@*/
int bar (int x) {
    int y = x > 0 ? x : -x;
    return y;
}