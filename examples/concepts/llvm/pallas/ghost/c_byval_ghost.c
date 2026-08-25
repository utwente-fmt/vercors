// Regression test to check that ghost-arguments that have the byval-attribute
// work as expected.

/*@
declare DEF_RESULT(int);
declare DEF_SEQ(int);
@*/


/*@
given SEQ(int) s;
requires _seqSize(int)(s) > 1;
ensures  _seqSize(int)(s) > 1;
@*/
int foo(int i) {
    int tmp = i * 6 * 7;
    /*@
    assert _seqSize(int)(s) > 0;
    @*/
    return tmp;
}
