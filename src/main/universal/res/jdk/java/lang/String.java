package java.lang;

// TODO: Should not have to use instanceof anywhere in this file
/*@
ghost
ensures \result != null ** \result instanceof String ** \result.data() == data;
ensures (\forall string otherData; true; (otherData == data) == (\result == internString(otherData)));
pure static final String internString(string data);

ghost
ensures \result != null ** \result instanceof String;
pure static final String concatStrings(String a, String b);
@*/

public class String {
    /*@
    pure string data();
    */

    //@ ensures \result == of(data());
    public native /*@ pure */ String intern();
}