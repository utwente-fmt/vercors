// Simple demo of a Pallas function-contract that is expected to fail.

/*@
 requires a >= 0 && b >= 0;
 ensures a > 0;
 @*/
int foo (int a, int b) {
    int x = a + b;
    x ++;
    return x * b + 1;
}
