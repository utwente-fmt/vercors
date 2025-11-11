#include <stdint.h>

struct B {
    int e;
    float f;
   char g;
};

typedef struct B B;

struct A {
    struct B a;
    char b;
    int c;
    float d;
};

typedef struct A A;


/*@ requires s != NULL;@*/
void test1(struct A *s) {
    /*@ assert (uintptr_t)s == (uintptr_t)&s->a;@*/
}

/*@ requires s != NULL;@*/
void test2(struct A *s) {
    /*@ assert (uintptr_t)&s->a < (uintptr_t)&s->b; @*/
}

/*@ requires s != NULL;@*/
void test3(struct A *s) {
    /*@ assert (uintptr_t)&s->b < (uintptr_t)&s->c; @*/
}

/*@ requires s != NULL;@*/
void test4(struct A *s) {
    /*@ assert (uintptr_t)&s->c < (uintptr_t)&s->d; @*/
}

/*@ requires s != NULL;@*/
void test5(struct A *s) {
    /*@ assert (uintptr_t)&s->a == (uintptr_t)&s->a.e; @*/
}

/*@ requires s != NULL;@*/
void test6(struct A *s) {
    /*@ assert (uintptr_t)&s->a.e < (uintptr_t)&s->b; @*/
}

/*@ requires s != NULL;@*/
void test7(struct A *s) {
    /*@ assert (uintptr_t)&s->a.f < (uintptr_t)&s->c; @*/
}

/*@ requires s != NULL;@*/
void test8(struct A *s) {
    /*@ assert (uintptr_t)&s->a.g < (uintptr_t)&s->d; @*/
}

