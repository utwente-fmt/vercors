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
    // Special constructor needed for constructing literals. Used by VerCors internally
    /*@
    ghost
    decreases;
    ensures data() == str;
    String(string str) {
        //@ assume false;
    }
    */

    String(String other) {
        // TODO (RR): Assume empty string here somehow...? Need spec syntax for strings
    }

    //@ requires other != null;
    String(String other) {
        //@ assume data() == other.data();
    }

    /*@
    decreases;
    pure string data();
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
}