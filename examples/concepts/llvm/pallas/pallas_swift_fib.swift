// Transformation requires the mem2reg-option
// Verification requires the --pallas-sroa option

// Recursive implementation of the fibonacci sequence.
/*@
pure;
requires n >= 0;
@*/
func fibRec(_ n: Int) -> Int {
    if (n == 0) {
        return 0
    } else if (n == 1) {
        return 1
    } else {
        return fibRec(n - 1) + fibRec(n - 2)
    }
}

// Iterative implementation of the fibonacci sequence.
/*@
requires n >= 0;
ensures _result() == fibRec(n);
@*/
func fibIt(_ n: Int) -> Int {
     if (n == 0) {
        return 0
     }  else if (n == 1) {
        return 1
     }

    var prevRes = 0
    var res = 1
    var i = 2

    /*@
    loop_invariant 2 <= i &&& i <= n+1;
    loop_invariant res == fibRec(i-1);
    loop_invariant prevRes == fibRec(i-2);
    @*/
    while i <= n {
        let tmp = prevRes + res
        prevRes = res
        res = tmp
        i += 1
    }
    return res
}
