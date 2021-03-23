// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases LabeledWhile
//:: tools silicon carbon
//:: verdict Pass

class C {
    int i;

    //@ requires Perm(i, write);
    void m1() {
        i = 0;
        //@ loop_invariant Perm(i, write) ** 0 <= i ** i <= 5;
        l: while (i < 5) {
            i += 1;
        }
        //@ assert i == 5;
        //@ assert Perm(i, write);
    }

    //@ requires Perm(i, write);
    void m2() {
        i = 100;
        //@ loop_invariant Perm(i, write) ** 100 <= i ** i <=110;
        l1: l2: while (i < 110) {
            i += 1;
        }
        //@ assert i == 110;
        //@ assert Perm(i, write);
    }

    //@ requires Perm(i, write);
    void m3() {
        i = 200;
        //@ loop_invariant Perm(i, write);
        l1: l2:
        //@ loop_invariant i <= 230;
        while (i < 230) {
            i += 1;
        }
        //@ assert i == 230;
        //@ assert Perm(i, write);
    }

    //@ requires Perm(i, write);
    void m4() {
        i = -7;
        //@ loop_invariant Perm(i, 1\2);
        l1:
        //@ loop_invariant Perm(i, 1\2);
        l2:
        //@ loop_invariant i <= 7;
        while (i < 7) {
            i += 1;
        }
        //@ assert i == 7;
        //@ assert Perm(i, write);
    }
}