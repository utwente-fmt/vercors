/*@
declare DEF_BV(int);
declare DEF_RESULT(int);
@*/


/*@
requires arr != NULL && n > 2;
requires ptr_block_length(arr) == n && ptr_block_offset(arr) == 0;
requires sep(Perm(arr+0, fracOf(1, 1)),
             Perm(arr+1, fracOf(1, 1)));
ensures ptr_block_length(arr) == n && ptr_block_offset(arr) == 0;
ensures sep(Perm(arr+0, fracOf(1, 1)),
            Perm(arr+1, fracOf(1, 1)));
ensures arr[0] == 0 && arr[1] == 0;
@*/
void bar(int* arr, int n) {
    arr[0] = 0;
    arr[1] = 0;
}


/*@
requires arr != NULL && n > 2;
requires ptr_length(arr) == n;
requires forallS(_and(0 <= BV(int)("i"),
                           BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 1)));
ensures ptr_length(arr) == n;
ensures forallS(_and(0 <= BV(int)("i"),
                          BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 1)));
ensures exists(_and(0 <= BV(int)("i"),
                         BV(int)("i") < n), arr[BV(int)("i")] == 0);
@*/
void foo(int* arr, int n) {
    arr[0] = 0;
}