// Tests loop invariants in C-programs.
// Simple test for for-loops without pointers, arrays, ...

/*@
declare DEF_RESULT(int);
@*/

/*@
requires n >= 0;
requires k >= 0;
ensures RESULT(int)() == n * k;
@*/
int mult(int n, int k) {
    int res = 0;

    /*@
    loop_invariant _and(0 <= i, i <= k);
    loop_invariant res == i * n;
    @*/
    for (int i = 0; i < k; i++) {
        res += n;
    }

    return res;
}