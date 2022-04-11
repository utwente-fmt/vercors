package A;

import static B.OtherClass.*;

class MainClass {
    void m() {
        //@ assert CONST == 10 || CONST != 10;
    }
}
