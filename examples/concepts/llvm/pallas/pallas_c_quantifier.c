/*@
declare DEF_BV(int);
declare DEF_RESULT(int);
@*/


/*@
requires arr != NULL && n > 2;
requires _ptr_block_length(arr) == n && _ptr_block_offset(arr) == 0;
requires _sep(_Perm(arr+0, _write),
              _Perm(arr+1, _write));
ensures _ptr_block_length(arr) == n && _ptr_block_offset(arr) == 0;
ensures _sep(_Perm(arr+0, _write),
             _Perm(arr+1, _write));
ensures arr[0] == 0 && arr[1] == 0;
@*/
void bar(int* arr, int n) {
    arr[0] = 0;
    arr[1] = 0;
}


/*@
requires arr != NULL && n > 2;
requires _ptr_length(arr) == n;
requires _forallS(_and(0 <= _bv(int, i),
                            _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _write));
ensures _ptr_length(arr) == n;
ensures _forallS(_and(0 <= _bv(int, i),
                           _bv(int, i) < n), _Perm(&arr[_bv(int, i)], _write));
ensures _exists(_and(0 <= _bv(int, i),
                          _bv(int, i) < n), arr[_bv(int, i)] == 0);
@*/
void foo(int* arr, int n) {
    arr[0] = 0;
}