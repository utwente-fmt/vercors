/*
Test that predicates are supported in LLVM.
*/

/*@
declare DEF_BV(int);
declare DEF_UNFOLDING(bool);
@*/

/*@
predicate arrWrite(int *a, int n) := a != NULL &&
                                     _ptr_length(a) >= n &&
                                     _forallS(_inRange(0, _bv(int, i), n),
                                              _Perm(&a[_bv(int, i)], _write));
predicate arrZero(int *a, int n) := a != NULL &&
                                    _ptr_length(a) >= n &&
                                    _sep(_forallS(_inRange(0, _bv(int, i), n),
                                                  _Perm(&a[_bv(int, i)], _write)),
                                         _forall(_inRange(0, _bv(int, i), n),
                                                 a[_bv(int, i)] == 0));
@*/

/*@
requires size >= 0;
requires arrWrite(arr, size);
ensures arrZero(arr, size);
@*/
void zero_arr(int *arr, int size) {
    /*@
    loop_invariant 0 <= i && i <= size;
    loop_invariant arrWrite(arr, size);
    loop_invariant _unfolding(bool)(arrWrite(arr, size), 
                        _forall(_inRange(0, _bv(int, j), i),
                                            arr[_bv(int, j)] == 0));
    @*/
    for (int i = 0; i < size; ++i) {
        /*@ 
        unfold arrWrite(arr, size); 
        @*/
        arr[i] = 0;
        /*@ 
        fold arrWrite(arr, size); 
        @*/
    }

    /*@ 
    unfold arrWrite(arr, size);
    fold arrZero(arr, size); 
    @*/
}