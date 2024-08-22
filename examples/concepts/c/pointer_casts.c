#include <stdbool.h>

struct A {
    int integer;
    bool boolean;
};

struct B {
    struct A struct_a;
};

void canCastToInteger() {
    struct B struct_b;
    struct_b.struct_a.integer = 5;
    int *pointer_to_integer = (int *)&struct_b;
    //@ assert *pointer_to_integer == 5;
    //@ assert pointer_to_integer == &struct_b.struct_a.integer;
    //@ assert pointer_to_integer == (int *)&struct_b.struct_a;
    // The following is not implemented yet
    // assert pointer_to_integer == &struct_b
    // assert pointer_to_integer == &struct_b.struct_a
    *pointer_to_integer = 10;
    //@ assert struct_b.struct_a.integer == 10;
}

void cannotCastToBoolean() {
    struct B struct_b;
    struct_b.struct_a.boolean = true == true; // We currently don't support boolean literals
    // TODO: Do proper type checks for casts
    bool *pointer_to_boolean = (bool *)&struct_b;
    /*[/expect ptrPerm]*/
    //@ assert *pointer_to_boolean == 5;
    /*[/end]*/
    //@ assert pointer_to_boolean == &struct_b.struct_a.boolean;
    //@ assert pointer_to_boolean == (bool *)&struct_b.struct_a;
}

void castRemainsValidInLoop() {
    struct B struct_b;
    struct_b.struct_a.integer = 10;

    int *pointer_to_integer = (int *)&struct_b;

    //@ loop_invariant 0 <= i && i <= 10;
    //@ loop_invariant Perm(&struct_b, write);
    //@ loop_invariant Perm(struct_b, write);
    //@ loop_invariant pointer_to_integer == (int *)&struct_b;
    //@ loop_invariant *pointer_to_integer == 10 - i;
    for (int i = 0; i < 10; i++) {
        *pointer_to_integer = *pointer_to_integer - 1;
    }

    //@ assert struct_b.struct_a.integer == 0;
}

//@ requires a != NULL;
//@ context Perm(a, write);
//@ ensures *a == \old(*a) + 1;
void increaseByOne(int *a) {
    *a += 1;
}

void callWithCast() {
    struct B struct_b;
    struct_b.struct_a.integer = 15;

    int *pointer_to_integer = (int *)&struct_b;
    increaseByOne(pointer_to_integer);

    //@ assert struct_b.struct_a.integer == 16;
}
