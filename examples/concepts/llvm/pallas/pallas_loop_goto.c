// Regression-test to ensure that the bug, where an if without an else-branch
// in front of a loop caused an incorrect encoding, is fixed.
// Transform with -mem2reg option
/*@ declare DEF_RESULT(int); @*/

/*@ pure;
    requires n >= 0; @*/
int fibRec(int n) {
    if (n <= 1) { return n; } else { return fibRec(n-1) + fibRec(n-2); }
}

/*@ requires n >= 0;
    ensures RESULT(int)() == fibRec(n); @*/
int fibIt(int n) {
    if (n <= 1) { return n;}
    int prevRes = 0;
    int res = 1;

    /*@
    loop_invariant _and(2 <= i, i <= n+1);
    loop_invariant res == fibRec(i-1);
    loop_invariant prevRes == fibRec(i-2);
    @*/
    for (int i = 2; i <= n; i++) {
        int tmp = prevRes + res;
        prevRes = res;
        res = tmp;
    }
    return res;
}
