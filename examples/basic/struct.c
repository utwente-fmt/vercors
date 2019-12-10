void test() {
    int *x = NULL;
}

struct A {
    int x;
    int y;
};

typedef struct A Adef, Adef2;

typedef struct B {
    int x;
    int y;
} Bdef;

typedef struct C {
    int x;
    int y;
} *Cpdef;

typedef struct {
    int x;
    int y;
} Anondef;


/*@ context Perm(adef->x, write);
@*/
void otherMethod(Adef *adef) {
}

/*@ context Perm(adef->x, write);
@*/
void method(struct A *a, Adef *adef, struct B *b, Bdef *bdef, struct C *c, Cpdef cpdef, Anondef *anondef) {
    otherMethod(adef);
}