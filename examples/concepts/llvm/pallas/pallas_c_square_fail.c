// Tests failing loop invariants in C-programs.

/*@
declare DEF_RESULT(int);
@*/

/*@
requires n >= 0;
ensures RESULT(int)() == n * n;
@*/
int bad_square(int n) {
    int res = 0;

    /*@
    loop_invariant 0 <= i;
    loop_invariant i <= n;
    loop_invariant res == i * n;
    @*/
    for (int i = 0; i <= n; i++) {
        res += n;
    }

    return res;
}