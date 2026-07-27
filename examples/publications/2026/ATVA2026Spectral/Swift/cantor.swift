// Transform with the mem2reg-option
// Verify with VerCors-flag --pallas-sroa

/*@
pure;
requires true;
@*/
func triangular(n: Int) -> Int {
    return (n * (n + 1)) / 2
}

/*@
pure;
ensures _result() == n * n;
@*/
func square(n: Int) -> Int {
    return n * n
}

/*@
pure;
ensures y == 0 ==> _result() == triangular(n: x);
@*/
func cantorPair(x: Int, y: Int) -> Int {
    return (square(n: x + y) + x + (3 * y)) / 2
}