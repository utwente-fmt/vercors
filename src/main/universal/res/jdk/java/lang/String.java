package java.lang;

// TODO: Should not have to use instanceof anywhere in this file

/*@
// adt goes brrr
ghost
adt StringBijection {
    pure String toString(string data);
    pure string fromString(String object);

    axiom (\forall string data; {: toString(data) :} != null);
    axiom (\forall String o; o != null; toString({: fromString(o) :}) == o);
    axiom (\forall string data; fromString({: toString(data) :}) == data);
}

// Interface function - vercors looks for this at verification time, and uses it to imlement interning semantics from the JLS
// This could be overridden by users if they want something different
ghost
decreases assume;
pure String internToString(string data) = StringBijection.toString(data);

// Interface function - vercors looks for this at verification time, and uses it to implement string concatenation
// In the future we should probably add a more meaningful contract w.r.t. the data member of the string
ghost
decreases;
ensures \result != null ** \result instanceof String;
pure String concatStrings(String a, String b);
@*/

public class String {
    /*@
    decreases;
    pure string data();
    */

    //@ decreases;
    //@ ensures \result == internToString(data());
    public native /*@ pure */ String intern();

    /*@
    ghost
    decreases;
    public pure boolean isEmpty();
     @*/
}