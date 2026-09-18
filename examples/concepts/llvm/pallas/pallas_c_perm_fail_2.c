#include <stdint.h>
// Test that the permission-annotations of Pallas work as expected.
// Expects a fail due to lacking permission to a field in a struct

typedef struct S {
    int64_t a, b, c, d, e, f, g;
} BigStruct;

/*@
requires s != NULL;
ensures s->a == 0;
@*/
void bar(BigStruct *s) {
    s->a = 0;
}