// Test that the assumed-flag works correctly for generated contracts.

/*@
declare DEF_RESULT(int);
@*/

/*@
assumed contract for generated src foo;
ensures _result(int) == 2;
@*/


// This function is provided with an assumed contract
// that does not match the implementation.
int foo() {
    int i = 0;
    i += 3;
    i *= 2;
    return i;
}


/*@
ensures _result(int) == 3;
@*/
int main() {
    int a = 1;
    int b = foo();
    return a + b;
}