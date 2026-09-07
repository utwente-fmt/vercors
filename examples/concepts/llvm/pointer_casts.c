#include <stdbool.h>

struct A {
    int integer;
    bool boolean;
};

struct B {
    struct A struct_a;
};

typedef struct A A;
typedef struct B B;

/*@
declare DEF_OLD(int);
@*/

void canCastToInteger() {
    struct B struct_b;
    struct_b.struct_a.integer = 5;
    int *pointer_to_integer = (int *)&struct_b;
    /*@ assert *pointer_to_integer == 5;
     assert pointer_to_integer == &struct_b.struct_a.integer;
     assert pointer_to_integer == (int *)&struct_b.struct_a; @*/
    *pointer_to_integer = 10;
    /*@ assert struct_b.struct_a.integer == 10; @*/
}


void castRemainsValidInLoop() {
    struct B struct_b;
    struct_b.struct_a.integer = 10;

    int *pointer_to_integer = (int *)&struct_b;

    /*@ loop_invariant 0 <= i && i <= 10;
        loop_invariant pointer_to_integer == (int *)&struct_b;
        loop_invariant _Perm(&struct_b.struct_a.integer, _fracOf(1, 1));
        loop_invariant *pointer_to_integer == 10 - i; @*/
    for (int i = 0; i < 10; i++) {
        *pointer_to_integer = *pointer_to_integer - 1;
    }

    /*@ assert struct_b.struct_a.integer == 0; @*/
    struct_b.struct_a.integer = 10;

    // We can also specify the permission through the pointer
    /*@ loop_invariant 0 <= j && j <= 10;
        loop_invariant pointer_to_integer == (int *)&struct_b;
        loop_invariant _Perm(pointer_to_integer, _fracOf(1, 1));
        loop_invariant *pointer_to_integer == 10 - j; @*/
    for (int j = 0; j < 10; j++) {
        *pointer_to_integer = *pointer_to_integer - 1;
    }

    /*@ assert struct_b.struct_a.integer == 0; @*/
}

/*@ requires a != NULL;
requires _Perm(a, _fracOf(1,1));
ensures _Perm(a, _fracOf(1,1));
ensures *a == _old(int)(*a) + 1; @*/
void increaseByOne(int *a) {
    *a += 1;
}

void callWithCast() {
    struct B struct_b;
    struct_b.struct_a.integer = 15;

    int *pointer_to_integer = (int *)&struct_b;
    increaseByOne(pointer_to_integer);

    /*@ assert struct_b.struct_a.integer == 16; @*/
}
