/**
 * Test of assume-statement in Pallas.
 * Expected to pass, translate with --mem2reg
 */

/*@
declare DEF_RESULT(int);
@*/

/*@
requires a > 0 && b > 0;
@*/
int foo(int a, int b) {
    int tmp = a;
    // This assumption is unsound
    /*@
    assume tmp < 0;
    @*/
    if (a > b) {
        a++;
    } else {
        b++;
    }
    /*@ assert false; @*/
    tmp += b;
    return a + b;
}