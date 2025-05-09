/**
 * Regression-test for the bug where variables, for which the following criteria 
 * hold caused the verification to crash: 
 * - The variable is not used in the loop
 * - The Variable is assigned more than once
 * Transform with --mem2reg
 */

/*@ 
requires n >= 0;
@*/
int foo(int n) {
    int oldN = n;
    oldN += 1;
    oldN -= 1;
    if (n < 42) {
        return n;
    }
    int res = 0;
    /*@
    loop_invariant 0 <= i && i <= n + 1;
    loop_invariant res >= 0 ;
    loop_invariant oldN == n;
    @*/
    for (int i = 0; i <= n; i++) {
        res += i;
    }
    return res;
}
