#include <stdint.h>
// Test that the permission-annotations of Pallas work as expected.

/*@
requires ptr != NULL && Perm(ptr, fracOf(1, 2));
ensures Perm(ptr, fracOf(1, 2));
@*/
int foo(int *ptr) {
    return *ptr + 5;
}

typedef struct S {
    int64_t a, b, c, d, e, f, g;
} BigStruct;

/*@
requires s != NULL && Perm(s, fracOf(1, 2));
requires sep(Perm(&s->a, fracOf(1, 1)), Perm(&s->b, fracOf(1, 1)));
ensures Perm(s, fracOf(1, 2));
ensures sep(Perm(&s->a, fracOf(1, 1)), Perm(&s->b, fracOf(1, 1)));
ensures s->a == 0 && s->b == 0;
@*/
void bar(BigStruct *s) {
    s->a = 0;
    s->b = 0;
}