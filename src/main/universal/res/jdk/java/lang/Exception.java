package java.lang;

public class Exception extends Throwable {
    Exception() {
        //@ assume false;
    }

    //@ ensures getMessage() == msg;
    Exception(String msg) {
        //@ assume false;
    }
    public /*@ pure @*/ String getMessage();
    public void printStackTrace();
}