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

public /*@ builtin_String @*/ class String {
    // Special constructor needed for constructing literals. Used by VerCors internally
    /*@
    ghost
    decreases
    ensures this.data() == data;
    String(string data);
    */

    //@ requires other != null;
    String(String other) {
        //@ assume data() == other.data()
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
    ensures \result != null ** \result instanceof String;
    ensures \result.data() == data() + other.data();
    String +(String other) {
        return new String(data() + other.data());
    }

    decreases;
    ensures \result != null ** \result instanceof String;
    ensures \result.data() == data() + other.data();
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