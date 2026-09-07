/*@
declare DEF_BV(int);
@*/

/*@
requires arr != NULL && n > 2;
requires _ptr_length(arr) == n;
requires _forallS(_and(0 <= _bv(int, i),
                            _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _write));
ensures _ptr_length(arr) == n;
ensures _forallS(_and(0 <= _bv(int, i),
                           _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _write));
ensures _exists(_and(0 <= _bv(int, i),
                          _bv(int, i) < n), arr[_bv(int, i)] == 1);
@*/
void foo(int* arr, int n) {
    arr[0] = 0;
    arr[1] = 2;
}