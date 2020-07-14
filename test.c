/*@
requires \pointer(a, 10, write);
*/
void test(int *a) {
    //@ assert \pointer(a, 10, write);
    //@ assert \pointer(a, 8, write);
    //@ assert (\forall* int i; 0 <= i && i < 10; \pointer_index(a, i, write));
    int *b = a+5;
    //@ assert a[5] == b[0];
    //@ assert \pointer(b, 5, write);
}
