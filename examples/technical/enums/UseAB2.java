package other.project;

import some.pkg.AB;
import static some.pkg.AB.*;
import java.lang.annotation.RetentionPolicy;

class UseAB2 {
    void foo() {
        // RetentionPolicy xx;
        //@ assert RetentionPolicy.SOURCE != null;
        AB ab = A;
        //@ assert ab != null;
        //@ assert ab != B;
    }

    void bar(AB ab) {
        //@ assert ab != null ==> ab == A || ab == B;
    }
}