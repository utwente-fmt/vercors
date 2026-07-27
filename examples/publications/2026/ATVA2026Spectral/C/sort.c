// Transform with mem2reg-option
/*@
declare DEF_BV(int);
declare DEF_RESULT(int);
declare DEF_OLD(int);
@*/

/*@
requires arr != NULL;
requires 0 <= startIdx && startIdx < endIdx && endIdx <= _ptr_length(arr);
requires _forallS(_inRange(startIdx, _bv(int, i), endIdx),
                  _Perm(&arr[_bv(int, i)], _fracOf(1, 2)));
ensures _forallS(_inRange(startIdx, _bv(int, i), endIdx),
                 _Perm(&arr[_bv(int, i)], _fracOf(1, 2)));
ensures _inRange(startIdx, _result(int), endIdx);
ensures _forall(_inRange(startIdx, _bv(int, i), endIdx),
                arr[_result(int)] <= arr[_bv(int, i)]);
@*/
int getMinIdx(int *arr, int startIdx, int endIdx) {
    int minIdx = startIdx;
    /*@
    loop_invariant _inRange(startIdx, idx, endIdx + 1);
    loop_invariant _inRange(startIdx, minIdx, endIdx);
    loop_invariant _forallS(_inRange(startIdx, _bv(int, i), endIdx),
                            _Perm(&arr[_bv(int, i)], _fracOf(1, 2)));
    loop_invariant _forall(_inRange(startIdx, _bv(int, i), idx),
                           arr[minIdx] <= arr[_bv(int, i)]);
    @*/
    for (int idx = startIdx + 1; idx < endIdx; idx++) {
        if (arr[idx] < arr[minIdx]) {
            minIdx = idx;
        }
    }
    return minIdx;
}

/*@
requires arr != NULL;
requires _inRange(0, idx1, _ptr_length(arr));
requires _inRange(0, idx2, _ptr_length(arr));
requires idx1 != idx2;
requires _sep(_Perm(&arr[idx1], _write),
              _Perm(&arr[idx2], _write));
ensures  _sep(_Perm(&arr[idx1], _write),
              _Perm(&arr[idx2], _write));
ensures arr[idx1] == _old(int)(arr[idx2]);
ensures arr[idx2] == _old(int)(arr[idx1]);
@*/
void swap(int *arr, int idx1, int idx2) {
    int tmp = arr[idx1];
    arr[idx1] = arr[idx2];
    arr[idx2] = tmp;
}

/*@
requires arr != NULL;
requires n > 0;
requires _ptr_length(arr) == n;
requires _forallS(_inRange(0, _bv(int, i), n), _Perm(&arr[_bv(int, i)], _write));
ensures  _forallS(_inRange(0, _bv(int, i), n), _Perm(&arr[_bv(int, i)], _write));
ensures  _forall (_and(_inRange(0, _bv(int, i), _bv(int, j)), _bv(int, j) < n),
                  arr[_bv(int, i)] <= arr[_bv(int, j)]);
@*/
void selectSort(int *arr, int n) {
    // Required to ensure that VerCors correctly infers the type of the arr-pointer.
    int first = arr[0];
    /*@
    loop_invariant 0 <= idx && idx < n;
    loop_invariant _forallS(_inRange(0, _bv(int, i), n), _Perm(&arr[_bv(int, i)], _write));
    loop_invariant _forall(_and(_inRange(0,   _bv(int, i), idx),
                                _inRange(idx, _bv(int, j), n)),
                           arr[_bv(int, i)] <= arr[_bv(int, j)]);
    loop_invariant _forall(_and(_inRange(0, _bv(int, i), _bv(int, j)), _bv(int, j) < idx),
                           arr[_bv(int, i)] <= arr[_bv(int, j)]);
    @*/
    for (int idx = 0; idx < n-1; ++idx) {
        int minIdx = getMinIdx(arr, idx, n);
        if (minIdx != idx) {
            swap(arr, idx, minIdx);
        }
    }
}