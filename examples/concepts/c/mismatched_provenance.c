#include <stdint.h>
#include <stdbool.h>

int failing() {
    int a[] = {5, 6, 7, 8};
    int b[] = {1, 2, 3, 4};
    uintptr_t c = (uintptr_t)&a[3];
    int *d = (int *)(c + 4);
    // The compiler is allowed to assume d==b includes checking for provenance (i.e. it may be false even if the adress is equal)
    if (d == b) {
        //@ assert (uintptr_t)d == (uintptr_t)b;
        //@ assert *d == 1;
        return 1;
    } else {
        // Since the provenance is not equal here we may not assume that the address of d and b are distinct when d != b
        /*[/expect assertFailed:false]*/
        //@ assert (uintptr_t)d != (uintptr_t)b;
        /*[/end]*/
        return 0;
    }
}


int passing() {
    int a[] = {5, 6, 7, 8};
    uintptr_t c = (uintptr_t)&a[2];
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

