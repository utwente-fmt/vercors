package java.lang;

import java.io.PrintStream;
import java.io.InputStream;

class System {
    public static final PrintStream out;
    public static final PrintStream err;
    public static final InputStream in;

    /*@
    ghost
    ensures out != null ** err != null;
    public void staticInvariant() {
        //@ assume false;
    }
    */

    //@ requires in != null;
    public static void setIn(InputStream in) {
        //@ assume false;
    }

    //@ requires out != null;
    public static void setOut(PrintStream out) {
        //@ assume false;
    }

    //@ requires err != null;
    public static void setErr(PrintStream err) {
        //@ assume false;
    }
}