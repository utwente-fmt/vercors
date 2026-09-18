struct A {
    int x;
};

//@ requires a != NULL ** Perm(*a, write);
//@ requires *a > -2147483618;
//@ requires Perm(y.x, write);
void foo(int *a, struct A y) {
    int b = *a - 10;

    //@ loop_invariant 0 <= i && i <= 10;
    for (int i = 0; i < 10; i++) {
    }

    //@ assert b == *a - 10;

    //@ assert -2147483647 - 1 <= y.x;
    //@ assert y.x <= 2147483647;
    b += 10;

    unsigned int c = -1;
    //@ assert c == 4294967295;
    unsigned int d = 4294967295U;
    d += 1;
    //@ assert d == 0;
    unsigned long long e = 4294967296ULL;
    // Implicit conversion, will do an unsigned overflow:
    bar(e);
}

//@ requires a == 0;
void bar(unsigned int a) {
}


