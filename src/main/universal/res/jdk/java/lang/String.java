package java.lang;

// TODO: Should not have to use instanceof anywhere in this file

/*@
ghost
adt StringBijection {
    pure String toString(string data);
    pure string fromString(String object);

    axiom (\forall string data; {: toString(data) :} != null);
    axiom (\forall String o; o != null; toString({: fromString(o) :}) == o);
    axiom (\forall string data; fromString({: toString(data) :}) == data);
}
@*/

public class String {
    /*@
    ghost
    decreases;
    ensures data() == str;
    String(string str) {
        //@ assume false;
    }
    @*/

    //@ requires other != null;
    String(String other) {
        //@ assume data() == other.data();
    }

    /*@
    decreases;
    pure string data();
    */

    /*@
    ghost
    decreases;
    ensures \result == StringBijection.toString(str);
    static pure String vercorsIntern(string str);
     */

    //@ decreases;
    //@ ensures \result == StringBijection.toString(data());
    public native /*@ pure @*/ String intern();


    /*@
    decreases;
    requires other != null;
    ensures \result != null ** \result instanceof String;
    ensures \result.data() == data() + other.data();
    String +(String other) {
        return new String(data() + other.data());
    }

    decreases;
    requires other != null;
    ensures \result != null ** \result instanceof String;
    ensures \result.data() == other.data() + data();
    String right+(String other) {
        return new String(other.data() + data());
    }
    @*/

    /*@
    ghost
    decreases;
    public pure boolean isEmpty();
     @*/


    // Since VerCors does not support Object... varargs, in order to be able to comfortably specific examples that use
    //  String.format, I am adding a few possible scenarios which could be used in verification.
    /*@
    ghost
    decreases;
    public static String format(String other, int arg1, String arg2) {
        return other;
    }
     @*/

    /*@
    ghost
    decreases;
    public static String format(String other, int arg1) {
        return other;
    }
     @*/

    /*@
    ghost
    decreases;
    public static String format(String other, int arg1, int arg2) {
        return other;
    }
     @*/

    /*@
    ghost
    decreases;
    public static String format(String other, String arg1, String arg2) {
        return other;
    }
     @*/

    /*@
    ghost
    decreases;
    public static String format(String other, String arg1) {
        return other;
    }
     @*/

}