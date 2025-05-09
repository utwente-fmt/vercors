// To be transformed with the mem2reg-option
// This tests that the phi-nodes are encoded correctly.
// I.e using the old (incorrect) encoding, this does not
// verify because the encoding was order-dependent.

/*@
declare DEF_RESULT(int);
@*/


// Recursive implementation of the fibonacci sequence.
/*@
pure;
requires n >= 0;
@*/
int fibRec(int n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fibRec(n - 1) + fibRec(n - 2);
    }
}

// Iterative implementation of the fibonacci sequence.
/*@
requires n >= 0;
ensures RESULT(int)() == fibRec(n);
@*/
int fibIt(int n) {
     if (n == 0) {
        return 0;
     }  else if (n == 1) {
        return 1;
     }

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