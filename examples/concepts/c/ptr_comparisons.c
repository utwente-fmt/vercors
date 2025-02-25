#include <stdint.h>

//@ requires x != NULL;
//@ context Perm(x, write);
void bar(int *x);

void foo() {
    int a[] = {1, 2, 3, 4};
    int *start = &a[0];
    int *p = start;
    int *end = &a[3];
    //@ assert end >= start;
    //@ assert start < end;
    //@ loop_invariant start <= p && p <= end;
    //@ loop_invariant (\forall* int i=0 .. 4; Perm(&a[i], write));
    while (p <= end) {

        if (p == end) {
            break;
        } else {
            //@ assert end > p;
        }
    }
}

