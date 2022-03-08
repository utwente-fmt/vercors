package java.lang;

// TODO: Should not have to use instanceof anywhere in this file

public class String {
    /*@
    pure seq<int> data();

    ghost
    ensures \result != null ** \result instanceof String ** \result.data() == data;
    ensures (\forall seq<int> otherData; true; (otherData == data) == (\result == of(otherData)));
    pure static final String of(seq<int> data);

    ghost
    ensures \result != null ** \result instanceof String;
    pure static final String operatorPlus(String a, String b);
    */

    //@ ensures \result == of(data());
    public native /*@ pure */ String intern();
}