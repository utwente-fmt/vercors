/*@
declare DEF_BV(int);
@*/

/*@
requires arr != NULL && n > 2;
requires ptr_length(arr) == n;
requires forallS(_and(0 <= BV(int)("i"),
                           BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 1)));
ensures ptr_length(arr) == n;
ensures forallS(_and(0 <= BV(int)("i"),
                          BV(int)("i") < n), Perm(&arr[BV(int)("i")], fracOf(1, 1)));
ensures exists(_and(0 <= BV(int)("i"),
                         BV(int)("i") < n), arr[BV(int)("i")] == 1);
@*/
void foo(int* arr, int n) {
    arr[0] = 0;
    arr[1] = 2;
}