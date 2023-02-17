package other.project;

import some.pkg.AB;
import static some.pkg.AB.*;

class UseAB2 {
    void foo() {
        AB ab = A;
        //@ assert ab != null;
        //@ assert ab != B;
    }

    void bar(AB ab) {
        //@ assert ab != null ==> ab == A || ab == B;
    }
}