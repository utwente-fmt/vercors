package java.lang;

public class Exception extends Throwable {
    Exception() {
    }

    //@ ensures getMessage() == msg;
    Exception(String msg) {
        //@ assume getMessage() == msg;
    }
    public /*@ pure @*/ String getMessage();
    public void printStackTrace();
}
