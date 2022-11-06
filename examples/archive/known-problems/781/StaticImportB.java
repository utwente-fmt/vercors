package B;

import static A.MainClass.myField;
import static A.MainClass.staticMethod;

class OtherClass {
    public static final int CONST = 10;

    //@ ensures \result == 33;
    public static int compute() {
        return 33;
    }

    void m() {
        int x = staticMethod();
        //@ assert x == 55;
        //@ assert myField == 25 || myField != 25;
    }
}