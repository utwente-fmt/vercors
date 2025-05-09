#include <stdint.h>
/*@
declare DEF_OLD(int);
@*/


// Expected to fail
/*@
requires iPtr != NULL && Perm(iPtr, fracOf(1, 1));
ensures  iPtr != NULL && Perm(iPtr, fracOf(1, 1));
ensures *iPtr == OLD(int)(*iPtr + 2);
@*/
void foo(int *iPtr) {
    *iPtr = *iPtr + 1;
}