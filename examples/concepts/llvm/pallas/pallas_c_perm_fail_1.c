// Test that the permission-annotations of Pallas work as expected.
// Expects a fail due to lacking permission.

/*@
requires ptr != NULL && _Perm(ptr, _fracOf(1, 4));
ensures _Perm(ptr, _fracOf(1, 2));
@*/
int foo(int *ptr) {
    return *ptr + 5;
}
