#include <stdint.h>
/*@
declare DEF_OLD(int);
@*/


// Expected to fail
/*@
requires iPtr != NULL && _Perm(iPtr, _fracOf(1, 1));
ensures  iPtr != NULL && _Perm(iPtr, _fracOf(1, 1));
ensures *iPtr == _old(int)(*iPtr + 2);
@*/
void foo(int *iPtr) {
    *iPtr = *iPtr + 1;
}