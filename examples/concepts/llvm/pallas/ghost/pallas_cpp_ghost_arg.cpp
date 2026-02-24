/**
 * Basic example of how ghost-arguments can be used in C++.
 */

/*@
declare using namespace pallasSpec;
@*/


/*@
inline predicate isMax(int max, int a, int b) := (max == a || max == b) && 
                                                 (max >= a && max >= b); 
inline predicate isMin(int min, int a, int b) := (min == a || min == b) && 
                                                 (min <= a && min <= b); 
@*/

/*@
given  int x;
yields bool both_gt_x;
yields int min;
requires a != nullptr && b != nullptr;
requires _sep(_Perm(a, _fracOf(1, 2)), 
              _Perm(b, _fracOf(1, 2)));
ensures _sep(_Perm(a, _fracOf(1, 2)), 
             _Perm(b, _fracOf(1, 2)));
ensures isMax(_result<int>(), *a, *b);
ensures isMin(min, *a, *b);
ensures both_gt_x == (*a > x && *b > x);
@*/
int get_max(int *a, int *b) {
    /*@
    ghost assign min = *a <= *b ? *a : *b;
    ghost assign both_gt_x = (*a > x && *b > x);
    @*/
    if (*a > *b) {
        return *a;
    } else {
        return *b;
    }
}

/**
 * Spectral requires all ghost variables to be mentioned at least once in the 
 * contract. Hence we need the two ensures-clauses even though they are 
 * trivially true. 
 */
/*@
yields int min;
yields bool both_gt;
ensures both_gt || !both_gt;
ensures min >= 0 || -min >= 0;
@*/
void run() {
    int a = 1;
    int b = 42;

    int max = get_max /*@ given x = 42; @*/ /*@ yields min = min; both_gt = both_gt_x; @*/ (&a, &b);
    /*@
    assert max == 42;
    assert min == 1;
    assert both_gt == false;
    @*/

    a = -1;
    b = 84;
    // Yields-bindings do not need to mention all variables
    get_max /*@ given x = -42; @*/ /*@ yields both_gt = both_gt_x; @*/ (&a, &b);

    /*@
    assert both_gt == true;
    @*/

}
