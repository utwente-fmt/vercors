// Test for loop-invariants in C with quantifiers and arrays.

/*@
declare DEF_BV(int);
declare DEF_RESULT(int);
@*/

/*@
requires arr != NULL && n > 0;
requires ptr_length(arr) == n;
requires forallS(_and(0 <= BV(int)("i"),
                           BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 2)));
ensures ptr_length(arr) == n;
ensures forallS(_and(0 <= BV(int)("i"),
                          BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 2)));
ensures forall(_and(0 <= BV(int)("i"),
                         BV(int)("i") < n), arr[BV(int)("i")] >= RESULT(int)());
@*/
int foo(int* arr, int n) {
    int idx = 1;
    int min = arr[0];

    /*@
    loop_invariant arr != NULL;
    loop_invariant _and(1 <= idx, idx <= n);
    loop_invariant ptr_length(arr) == n;
    loop_invariant forallS(_and(0 <= BV(int)("i"),
                                     BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 2)));
    loop_invariant forall(_and(0 <= BV(int)("i"),
                                    BV(int)("i") < idx), arr[BV(int)("i")] >= min);
    @*/
    while (idx < n) {
        if (arr[idx] < min)
            min = arr[idx];
        idx++;
    }

    return min;
}