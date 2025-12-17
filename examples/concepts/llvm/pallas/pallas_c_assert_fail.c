/**
 * Test of assert-statements in Pallas.
 * Expected to fail, translate with --mem2reg
 */

/*@
declare DEF_RESULT(int);
@*/

/*@
requires a > 0 && b > 0;
ensures  _result(int) > 0;
@*/
int foo (int a, int b) {
    int tmp = a;
    /*@ assert tmp > 0; @*/
    if (a > b) {
        a++;
    } else {
        b++;
    }
    /*@ assert tmp > a + b; @*/
    tmp += b;
    return a + b;
}