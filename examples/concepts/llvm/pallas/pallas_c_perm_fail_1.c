// Test that the permission-annotations of Pallas work as expected.
// Expects a fail due to lacking permission.

/*@
requires ptr != NULL && Perm(ptr, fracOf(1, 4));
ensures Perm(ptr, fracOf(1, 2));
@*/
int foo(int *ptr) {
    return *ptr + 5;
}
