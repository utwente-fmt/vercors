// Based on the example program P_2 in the paper Unification Based Pointer Analysis without Oversharing by Kuderski et al.

const int INT_TAG = 0, FLOAT_TAG = 1;
struct Element { int tag; };
struct IElement { struct Element e; int *d; };
struct FElement { struct Element e; float *d; };

void print_int(int v);

//@ context_everywhere Perm(INT_TAG, read);
//@ context_everywhere Perm(FLOAT_TAG, read);
//@ context_everywhere INT_TAG != FLOAT_TAG;
void baz() {
    int a = 1; // o6
    float f; // o7
    struct IElement e1;
    e1.e.tag = INT_TAG;
    e1.d = &a;
    struct FElement e2;
    e2.e.tag = FLOAT_TAG;
    e2.d = &f; // w
    //@ assert Perm(e1.d, write);
    //@ assert Perm(e2.d, write);
    struct Element *elems[2] = {(struct Element *)&e1, (struct Element *)&e2}; // x

    //@ loop_invariant 0 <= i && i <= 2;
    //@ loop_invariant elems != NULL;
    //@ loop_invariant \pointer(elems, 2, write);
    //@ loop_invariant Perm(&e1, write);
    //@ loop_invariant Perm(e1, write);
    //@ loop_invariant e1.d != NULL;
    //@ loop_invariant Perm(e1.d, write);
    //@ loop_invariant Perm(&e2, write);
    //@ loop_invariant Perm(e2, write);
    //@ loop_invariant e2.d != NULL;
    //@ loop_invariant Perm(e2.d, write);
    //@ loop_invariant elems[0] == (struct Element *)&e1;
    //@ loop_invariant elems[1] == (struct Element *)&e2;
    //@ loop_invariant elems[0]->tag == INT_TAG;
    //@ loop_invariant elems[1]->tag == FLOAT_TAG;
    for (int i = 0; i < 2; ++i) {
        if (elems[i]->tag == INT_TAG) {
            struct IElement *ie = (struct IElement *)elems[i];
            int *ip = (int *) ie->d;
            print_int(*ip);
        }
    }
}
