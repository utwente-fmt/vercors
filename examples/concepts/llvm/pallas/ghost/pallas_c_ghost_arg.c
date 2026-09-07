#include <stdbool.h>

/*@
declare DEF_BV(int);
declare DEF_OLD(int);
@*/

/*@
given  int x;
yields bool all_gt_x;
requires n >= 0;
requires arr != NULL && _ptr_length(arr) >= n;
requires _forallS(_inRange(0, _bv(int, i), n), 
                  _Perm(&arr[_bv(int, i)], _write));
ensures  _forallS(_inRange(0, _bv(int, i), n), 
                  _Perm(&arr[_bv(int, i)], _write));
ensures  _forall (_inRange(0, _bv(int, i), n),
                  arr[_bv(int, i)] == 0);
ensures all_gt_x == _forall(_inRange(0, _bv(int, i), n),
                            _old(int)(arr[_bv(int, i)]) > x);
@*/
void clear_arr(int *arr, int n) {

    /*@
    ghost assign all_gt_x = true;
    @*/

    /*@
    loop_invariant 0 <= i && i <= n;
    loop_invariant _forallS(_inRange(0, _bv(int, j), n), 
                            _Perm(&arr[_bv(int, j)], _write));
    loop_invariant _forall (_inRange(0, _bv(int, j), i),
                            arr[_bv(int, j)] == 0);
    loop_invariant _forall (_inRange(i, _bv(int, j), n),
                            arr[_bv(int, j)] == _old(int)(arr[_bv(int, j)]));
    loop_invariant all_gt_x == _forall(_inRange(0, _bv(int, j), i),
                                       _old(int)(arr[_bv(int, j)]) > x);
    @*/
    for (int i = 0; i < n; ++i) {
        /*@
        ghost assign all_gt_x = all_gt_x && (arr[i] > x);
        @*/
        arr[i] = 0;
    }
}

/*@
yields bool res;
requires n > 0;
requires arr != NULL && _ptr_length(arr) >= n;
requires _forallS(_inRange(0, _bv(int, i), n),
                  _Perm(&arr[_bv(int, i)], _write));
requires _forall (_inRange(0, _bv(int, i), n),
                  arr[_bv(int, i)] == n+1);
ensures res == true;
@*/
void foo(int *arr, int n) {
    clear_arr /*@ given x = n; @*/ /*@ yields res = all_gt_x; @*/ (arr, n);

    /*@
    assert res == true;
    @*/
}