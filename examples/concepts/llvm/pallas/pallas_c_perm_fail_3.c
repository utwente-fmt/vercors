
/*@
requires iPtr != NULL && _Perm(iPtr, _fracOf(1, 1));
ensures _sep(_Perm(iPtr, _fracOf(2,3)), _Perm(iPtr, _fracOf(2,3)));
@*/
int foo(int *iPtr) {
    return *iPtr + 1;
}