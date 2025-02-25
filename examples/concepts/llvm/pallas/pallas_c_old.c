#include <stdint.h>
// Test that the old(...)-annotations of Pallas work as expected.

/*@
declare DEF_OLD(int);
@*/


// Test with regular values
/*@
requires iPtr != NULL && Perm(iPtr, fracOf(1, 1));
ensures  iPtr != NULL && Perm(iPtr, fracOf(1, 1));
ensures *iPtr == OLD(int)(*iPtr);
@*/
void foo(int *iPtr) {
    *iPtr = *iPtr + 1;
}

// Test with large struct-values.
typedef struct S {
    int64_t a, b, c, d, e, f, g;
} BigStruct;

// TODO:


/*
requires s != NULL && Perm(s, fracOf(1, 2));
requires sep(Perm(&s->a, fracOf(1, 1)), Perm(&s->b, fracOf(1, 1)));
ensures Perm(s, fracOf(1, 2));
ensures sep(Perm(&s->a, fracOf(1, 1)), Perm(&s->b, fracOf(1, 1)));
ensures s->a == 0 && s->b == 0;
*/
/*
void bar(BigStruct *s) {
    s->a = 0;
    s->b = 0;
}
*/