#include <stdint.h>
// Test that the permission-annotations of Pallas work as expected.

/*@
requires ptr != NULL && _Perm(ptr, _fracOf(1, 2));
ensures _Perm(ptr, _fracOf(1, 2));
@*/
int foo(int *ptr) {
    return *ptr + 5;
}

typedef struct S {
    int64_t a, b, c, d, e, f, g;
} BigStruct;

/*@
requires s != NULL;
requires _sep(_Perm(&s->a, _write), _Perm(&s->b, _write));
ensures _sep(_Perm(&s->a, _write), _Perm(&s->b, _write));
ensures s->a == 0 && s->b == 0;
@*/
void bar(BigStruct *s) {
    s->a = 0;
    s->b = 0;
}