// Transform with mem2reg-option

/*@
declare DEF_RESULT(int);
@*/

/*@
pure;
requires true;
@*/
int triangular(int n) {
    return (n * (n + 1)) / 2;
}

/*@
pure;
ensures _result(int) == n * n;
@*/
int square(int n) {
    return n * n;
}

/*@
pure;
ensures _imply(y == 0,
               _result(int) == triangular(x));
@*/
int cantorPair(int x, int y) {
    return (square(x + y) + x + (3 * y)) / 2;
}