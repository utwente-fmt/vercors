// Simple demo of a Pallas function-contract that is expected to verify.

/*@
 requires a >= 0 && b >= 0;
 ensures a >= -1 && b > -1;
 @*/
int foo (int a, int b) {


    return a * b + 1;
}


/*@
 requires x < 0;
@*/
int bar (int x) {
    int y = 1;
    y += x;
    y *= 42;
    return y;
}
