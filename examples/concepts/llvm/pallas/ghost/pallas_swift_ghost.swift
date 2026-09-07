/**
 * Basic example ghost-code in Swift.
 */

/*@
inline predicate isMax(_ max: Int, _ a: Int, _ b: Int) := (max == a || max == b) && 
                                                          (max >= a && max >= b); 
@*/

/*@
ghost function 
func my_min(_ x: Int, _ y: Int) -> Int { return x <= y ? x : y }
@*/


/*@
given  x: Int;
yields both_gt_x: Bool;
yields min: Int;
ensures isMax(_result(), a, b);
ensures min == my_min(a, b);
ensures both_gt_x == (a > x && b > x);
@*/
func get_max(_ a: Int, _ b: Int) -> Int {
    /*@
    ghost assign min = a <= b ? a : b;
    ghost assign both_gt_x = (a > x && b > x);
    @*/
    if a > b {
        return a;
    } 
    return b;
}

/**
 * Spectral requires all ghost variables to be mentioned at least once in the 
 * contract. Hence we need the two ensures-clauses even though they are 
 * trivially true. 
 */
/*@
yields min: Int;
yields both_gt: Bool;
ensures both_gt || !both_gt;
ensures min >= 0 || -min >= 0;
@*/
func run() -> () {
    var a: Int = 1
    var b: Int = 42

    let max = get_max /*@ given x = 42; @*/ /*@ yields min = min; both_gt = both_gt_x; @*/ (a, b)
    /*@
    assert max == 42;
    assert min == 1;
    assert both_gt == false;
    @*/

    a = -1
    b = 84
    // Yields-bindings do not need to mention all variables
    _ = get_max /*@ given x = -42; @*/ /*@ yields both_gt = both_gt_x; @*/ (a, b)

    /*@
    assert both_gt == true;
    @*/

}
