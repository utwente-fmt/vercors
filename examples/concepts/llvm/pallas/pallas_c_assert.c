/**
 * Test of assert-statements in Pallas.
 * Expected to pass, translate with --mem2reg
 */

/*@
declare DEF_RESULT(int);
@*/

/*@
requires a > 0 && b > 0 && c > 0;
ensures _result(int) >= (a * b) + c;
@*/
int bar(int a, int b, int c) {
    int tmp = 0;
    /*@
    loop_invariant 0 <= i && i <= b;
    loop_invariant tmp == i * a;
    @*/
    for (int i = 0; i < b; ++i) {
        tmp += a;
    }
    /*@
    assert tmp == a * b;
    @*/

    int tmp2 = tmp + c;
    /*@
    assert tmp2 = (a * b) + c;
    @*/
    return tmp2;
}

/*@
requires a > 0 && b > 0;
ensures  _result(int) > 0;
@*/
int foo(int a, int b) {
    int tmp = a;
    /*@ assert tmp > 0; @*/
    if (a > b) {
        a++;
    } else {
        b++;
    }
    /*@ assert tmp <= a; @*/
    tmp += b;
    return a + b;
}

int amazingFunctionWithSomeBranches(int a, int b) {
    int aVariable = a;
    if (a < 0) {
        aVariable *= -1;
        /*@
        assert aVariable == -a;
        @*/
        if ( b < 0) {
            aVariable *= -b;
        } else {
            aVariable *= b;
        }
        /*@
        assert aVariable >= 0;
        @*/
    } else {
        aVariable *= b;
    }

    /*@
    assert _imply(a < 0, 
                  aVariable >= 0);
    @*/
    return aVariable;
}