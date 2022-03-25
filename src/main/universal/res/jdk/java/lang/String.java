package java.lang;

// TODO: Should not have to use instanceof anywhere in this file
/*@
ghost
ensures \result != null ** \result instanceof String ** \result.data() == data;
ensures (\forall string otherData; true; (otherData == data) == (\result == internToString(otherData)));
pure String internToString(string data);

ghost
ensures \result != null ** \result instanceof String;
pure String concatStrings(String a, String b);
@*/

public class String {
    /*@
    pure string data();
    */

    //@ ensures \result == internToString(data());
    public native /*@ pure */ String intern();

    /*@
    ghost public pure boolean isEmpty();
     @*/
}