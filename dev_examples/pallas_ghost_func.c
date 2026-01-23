// Run from project root:
// ../pallas_spec2ir/build/bin/pallasSpec2ir dev_examples/pallas_ghost_func.c -o dev_examples/pallas_ghost_func.ll -m -lib ../pallas_spec2ir/res/spec_libs/c -lang C -wDir dev_examples/tmp/

/*@
declare DEF_RESULT(int);
@*/

/*@
ghost function
requires a >= 0 && b > 0;
int ghost_mult(int a, int b) {
    return a * b;
}
@*/

/*@
requires a >= 0 && b >= 0;
ensures _result(int) == ghost_mult(a, b);
@*/
int my_mult(int a, int b) {
    int res = 0;
    /*@
    loop_invariant 0 <= i && i <= b;
    loop_invariant res == i * a;
    @*/
    for (int i = 0; i < b; i++) {
        res += a;
    }
    return res;
}