package java.lang;

public class String {
    //@ pure seq<int> data();

    /*@
    ghost
    ensures \result != null ** \result.data() == data;
    ensures (\forall seq<int> otherData; true; (otherData == data) == (\result == of(otherData)));
    pure static String of(seq<int> data);
    */

    /*@
    ghost
    ensures \result == of(data());
    pure String intern();
    */
}