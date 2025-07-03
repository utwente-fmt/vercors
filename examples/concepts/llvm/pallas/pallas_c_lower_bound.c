// Test for loop-invariants in C with quantifiers and arrays.

/*@
declare DEF_BV(int);
declare DEF_RESULT(int);
@*/

/*@
requires arr != NULL && n > 0;
requires _ptr_length(arr) == n;
requires _forallS(_and(0 <= _bv(int, i),
                            _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _fracOf(1, 2)));
ensures _ptr_length(arr) == n;
ensures _forallS(_and(0 <= _bv(int, i),
                           _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _fracOf(1, 2)));
ensures _forall(_and(0 <= _bv(int, i),
                          _bv(int, i) < n), arr[_bv(int, i)] >= _result(int));
@*/
int foo(int* arr, int n) {
    int idx = 1;
    int min = arr[0];

    /*@
    loop_invariant arr != NULL;
    loop_invariant _and(1 <= idx, idx <= n);
    loop_invariant _ptr_length(arr) == n;
    loop_invariant _forallS(_and(0 <= _bv(int, i),
                                      _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _fracOf(1, 2)));
    loop_invariant _forall(_and(0 <= _bv(int, i),
                                     _bv(int, i) < idx), arr[_bv(int, i)] >= min);
    @*/
    while (idx < n) {
        if (arr[idx] < min)
            min = arr[idx];
        idx++;
    }

    return min;
}