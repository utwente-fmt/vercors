
/*@
requires iPtr != NULL && Perm(iPtr, fracOf(1, 1));
ensures sep(Perm(iPtr, fracOf(2,3)), Perm(iPtr, fracOf(2,3)));
@*/
int foo(int *iPtr) {
    return *iPtr + 1;
}