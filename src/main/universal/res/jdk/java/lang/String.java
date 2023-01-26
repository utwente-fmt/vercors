package java.lang;

// TODO: Should not have to use instanceof anywhere in this file
// TODO (RR): Use an adt and injectivity instead of the strange recursive forall in internToString

/*@
ghost
decreases assume;
ensures \result != null ** \result instanceof String ** \result.data() == data;
ensures (\forall string otherData; true; (otherData == data) == (\result == internToString(otherData)));
pure String internToString(string data);

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