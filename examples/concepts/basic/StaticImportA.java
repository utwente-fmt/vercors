package A;

import static B.OtherClass.*;

class MainClass {
    void m() {
        //@ assert CONST == 10 || CONST != 10;
        int x = compute();
        //@ assert x == 33;
    }

    //@ ensures \result == 55;
    public static int staticMethod() {
        return 55;
    }

    public static final int myField = 25;
}
