#include <stdint.h>
#include <stdbool.h>

int failing() {
    int a[] = {5, 6, 7, 8};
    int b[] = {1, 2, 3, 4};
    intptr_t c = (intptr_t)&a[3];
    int *d = (int *)(c + 4);
    // The compiler is allowed to assume d==b includes checking for provenance (i.e. it may be false even if the adress is equal)
    if (d == b) {
        // At runtime no such provenance check occurs so accessing *d is UB if we do not have provenance
        /*[/expect ptrPerm]*/
        //@ assert *d == 1;
        /*[/end]*/
        return 1;
    } else {
        return 0;
    }
}

// We can attempt some trickery to see if pointers are equal without knowing the provenance, but it won't work luckily
/*[/expect postFailed:false]*/
//@ requires (p == q) || (p != q);
//@ ensures \result == (p == q);
bool pointerEq(int *p, int *q) {
    return p == q;
}
/*[/end]*/

int passing() {
    int a[] = {5, 6, 7, 8};
    intptr_t c = (intptr_t)&a[2];
    int *d = (int *)(c + 4);
    if (d == a + 3) {
        // Here we assume that the pointer acquired through the integer to pointer cast has the same provenance as a
        // You can do this if you are sure that the compiler will also be able to figure this out (the exact behaviour of the compilers is not yet fully formalized)
        //@ assume \pointer_block(d) == \pointer_block(a);
        //@ assert *d == 8;
        /*[/expect assertFailed:false]*/
        //@ assert false;
        /*[/end]*/
        return 1;
    } else {
        return 0;
    }
}

