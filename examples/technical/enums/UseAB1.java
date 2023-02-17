package other.project;

import some.pkg.AB;

class UseAB1 {
    void foo() {
        AB ab = AB.A;
        //@ assert ab != null;
        //@ assert ab != AB.B;
    }

    void bar(AB ab) {
        //@ assert ab != null ==> ab == AB.A || ab == AB.B;
    }
}